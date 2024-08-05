use std::collections::{BTreeMap, BTreeSet};

use anyhow::Context;

#[derive(Debug)]
struct Grammar<'a> {
    nonterminals: BTreeSet<Nonterminal<'a>>,
    terminals: BTreeSet<Terminal<'a>>,
    rules: Vec<Rule<'a>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
struct Nonterminal<'a>(&'a str);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Terminal<'a> {
    Named(&'a str),
    Literal(&'a str),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum ProductionItem<'a> {
    Nonterminal(Nonterminal<'a>),
    Terminal(Terminal<'a>),
}

#[derive(Debug, PartialEq, Eq)]
struct Rule<'a> {
    nonterminal: Nonterminal<'a>,
    productions: Vec<ProductionItem<'a>>,
}

impl<'a> Grammar<'a> {
    fn parse(input: &'a str) -> anyhow::Result<Self> {
        let mut nonterminals = BTreeSet::new();
        let mut terminals = BTreeSet::new();
        let mut rules = Vec::new();
        let mut previous_nonterminal = None;

        for line in input.lines() {
            let line = line.trim();
            if line.starts_with("#") || line.is_empty() {
                continue;
            }

            let (nonterminal, rule_str) = if let Some(rule_str) = line.strip_prefix("|") {
                (
                    previous_nonterminal.context("No previous nonterminal for OR clause")?,
                    rule_str.trim(),
                )
            } else if let Some((nonterminal_str, rule_str)) = line.split_once("->") {
                let nonterminal = Nonterminal(nonterminal_str.trim());
                previous_nonterminal = Some(nonterminal);
                (nonterminal, rule_str.trim())
            } else {
                anyhow::bail!("Unknown production {line:?}");
            };

            nonterminals.insert(nonterminal);

            let mut productions = Vec::new();

            for prod in rule_str.split_whitespace() {
                let prod = prod.trim();

                if prod.starts_with("#") {
                    break;
                }

                let item = if let Some(literal) =
                    prod.strip_prefix("`").and_then(|x| x.strip_suffix("`"))
                {
                    let terminal = Terminal::Literal(literal);
                    terminals.insert(terminal);
                    ProductionItem::Terminal(terminal)
                } else if prod == r#""""# {
                    anyhow::ensure!(
                        productions.is_empty(),
                        r#"'""' in otherwise nonempty production rule"#
                    );
                    break;
                } else if prod.to_lowercase() == prod {
                    let nonterminal = Nonterminal(prod);
                    nonterminals.insert(nonterminal);
                    ProductionItem::Nonterminal(nonterminal)
                } else if prod.to_uppercase() == prod {
                    let terminal = Terminal::Named(prod);
                    terminals.insert(terminal);
                    ProductionItem::Terminal(terminal)
                } else {
                    anyhow::bail!("Unknown rule type {prod:?}");
                };

                productions.push(item);
            }

            rules.push(Rule {
                nonterminal,
                productions,
            });
        }

        Ok(Self {
            nonterminals,
            terminals,
            rules,
        })
    }

    /// Returns the first conflict `(item, rule)` where `rule` has non-disjoint
    /// predictive set (first + follow set) for its nonterminal because of `item`
    fn non_predictive(&self) -> Option<(&'_ ProductionItem<'a>, &'_ Rule<'a>)> {
        let mut first_sets: BTreeMap<_, _> = self
            .nonterminals
            .iter()
            .map(|&nt| (nt, BTreeSet::new()))
            .collect();

        let mut expand = self.nonterminals.clone();
        let mut follow = BTreeSet::new();

        while !expand.is_empty() {
            let mut next = BTreeSet::new();
            for &nt in &expand {
                for rule in self.rules.iter().filter(|x| x.nonterminal == nt) {
                    if let Some(item) = rule.productions.first() {
                        match item {
                            ProductionItem::Terminal(t) => {
                                first_sets.get_mut(&nt).unwrap().insert(*t);
                            }
                            ProductionItem::Nonterminal(other) => {
                                let items = first_sets[other].clone();
                                first_sets.get_mut(&nt).unwrap().extend(items.into_iter());
                                if expand.contains(&other) {
                                    next.insert(nt);
                                }
                            }
                        }
                    } else {
                        follow.insert(nt);
                    }
                }
            }

            if expand == next {
                panic!("Grammar is left recursive: dependencies are in {next:?}");
            }

            expand = next;
        }

        panic!("{first_sets:?}");

        None
    }
}

fn get_grammar() -> Grammar<'static> {
    Grammar::parse(include_str!("../grammar.txt")).expect("grammar is not valid")
}

#[test]
fn is_predictive() {
    let grammar = get_grammar();
    assert_eq!(grammar.non_predictive(), None, "grammar is not predictive");
}

#[test]
#[should_panic]
fn test_grammar_recursive() {
    let grammar = Grammar::parse(
        "a -> b
        b -> C
        | a
        c -> D
        | C
        ",
    )
    .unwrap();

    grammar.non_predictive();
}
