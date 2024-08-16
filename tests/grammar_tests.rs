use std::collections::{BTreeMap, BTreeSet};

use anyhow::Context;

#[derive(Debug)]
struct Grammar<'a> {
    nonterminals: BTreeSet<Nonterminal<'a>>,
    #[allow(dead_code)]
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

        if let Some(x) = rules.first() {
            let nt = x.nonterminal;
            rules
                .iter_mut()
                .filter(|r| r.nonterminal == nt)
                .for_each(|x| {
                    x.productions
                        .push(ProductionItem::Terminal(Terminal::Named("<end of file>")));
                })
        }

        Ok(Self {
            nonterminals,
            terminals,
            rules,
        })
    }

    /// Returns the predict sets (for each rule, FIRST union FOLLOW) for each rule.
    ///
    /// TODO: return left- and right-recursion dependency cycle sets instead of panicking/warning
    ///
    /// # Panics
    ///
    /// Panics if the grammar is **left-recursive**.
    fn predict_sets(&self) -> Vec<BTreeSet<Terminal<'a>>> {
        let mut first_sets: BTreeMap<_, _> = self
            .nonterminals
            .iter()
            .map(|&nt| (nt, BTreeSet::new()))
            .collect();

        let mut predict_sets = vec![BTreeSet::new(); self.rules.len()];

        let mut follow_sets: BTreeMap<_, _> = self
            .nonterminals
            .iter()
            .map(|&nt| (nt, BTreeSet::<Terminal>::new()))
            .collect();

        let mut expand = self.nonterminals.clone();

        while !expand.is_empty() {
            let mut next = BTreeSet::new();
            for &nt in &expand {
                for (i, rule) in self
                    .rules
                    .iter()
                    .enumerate()
                    .filter(|(_, x)| x.nonterminal == nt)
                {
                    let mut follow = true;
                    for item in &rule.productions {
                        if !follow {
                            break;
                        }

                        match item {
                            ProductionItem::Terminal(t) => {
                                first_sets.get_mut(&nt).unwrap().insert(Some(*t));
                                predict_sets[i].insert(Some(*t));
                                follow = false;
                            }
                            ProductionItem::Nonterminal(next_nt) => {
                                let mut next_first = first_sets[next_nt].clone();
                                follow = next_first.remove(&None);

                                predict_sets[i].extend(next_first.iter().copied());
                                first_sets.get_mut(&nt).unwrap().extend(next_first);

                                if expand.contains(next_nt) {
                                    next.insert(nt);
                                }
                            }
                        }
                    }

                    if follow {
                        first_sets.get_mut(&nt).unwrap().insert(None);
                        predict_sets[i].insert(None);
                    }
                }
            }

            if expand == next {
                panic!("Grammar is left recursive: dependency cycle: {next:?}\nfirst sets: {first_sets:?}");
            }

            expand = next;
        }

        expand = first_sets
            .iter()
            .filter_map(|(&nt, set)| set.contains(&None).then_some(nt))
            .collect();

        let mut changed = true;
        while changed {
            let mut next = BTreeSet::new();
            changed = false;

            for &nt in &expand {
                // In a predictive grammar, this would be at most one, but we don't know if the grammar
                // is predictive yet.
                let follow_rules: Vec<_> = self
                    .rules
                    .iter()
                    .enumerate()
                    .filter(|(i, r)| r.nonterminal == nt && predict_sets[*i].contains(&None))
                    .collect();

                for rule in &self.rules {
                    for (i, _) in rule
                        .productions
                        .iter()
                        .enumerate()
                        .filter(|(_, &x)| x == ProductionItem::Nonterminal(nt))
                    {
                        let mut follow = true;
                        for item in &rule.productions[i + 1..] {
                            if !follow {
                                break;
                            }
                            match item {
                                ProductionItem::Terminal(t) => {
                                    follow_sets.get_mut(&nt).unwrap().insert(*t);
                                    for (j, _) in &follow_rules {
                                        predict_sets[*j].insert(Some(*t));
                                    }
                                    follow = false;
                                }
                                ProductionItem::Nonterminal(other) => {
                                    let set = &first_sets[other];
                                    follow = set.contains(&None);
                                    let mut set: BTreeSet<_> =
                                        set.iter().filter_map(|x| *x).collect();
                                    if follow {
                                        if expand.contains(other) {
                                            next.insert(nt);
                                        }

                                        set.extend(follow_sets[other].iter().copied());
                                    }

                                    for (j, _) in &follow_rules {
                                        predict_sets[*j].extend(set.iter().map(|&x| Some(x)));
                                    }

                                    changed |= !follow_sets[&nt].is_superset(&set);
                                    follow_sets.get_mut(&nt).unwrap().extend(set);
                                }
                            };
                        }

                        if follow {
                            let set = follow_sets[&rule.nonterminal].clone();

                            for (j, _) in &follow_rules {
                                predict_sets[*j].extend(set.iter().map(|&x| Some(x)));
                            }

                            changed |= !follow_sets[&nt].is_superset(&set);
                            follow_sets.get_mut(&nt).unwrap().extend(set);

                            if expand.contains(&rule.nonterminal) {
                                next.insert(nt);
                            }
                        }
                    }
                }
            }

            if next == expand {
                eprintln!("Right-recursion detected:  dependency cycle: {next:?}.\nLooping until no change...");
            }

            expand = next;
        }

        predict_sets
            .into_iter()
            .map(|x| x.into_iter().flatten().collect())
            .collect()
    }

    fn non_predictive(&self) -> Vec<(usize, BTreeSet<Terminal<'a>>)> {
        let predict_sets = self.predict_sets();
        for (i, l) in predict_sets.iter().enumerate() {
            println!("{i}: {l:?}");
        }
        self.nonterminals
            .iter()
            .flat_map(|&nt| {
                self.rules
                    .iter()
                    .enumerate()
                    .filter(|(_, x)| x.nonterminal == nt)
                    .fold(
                        (BTreeSet::new(), Vec::new()),
                        |(set, mut errors), (i, _)| {
                            let intersection: BTreeSet<_> =
                                set.intersection(&predict_sets[i]).copied().collect();
                            if !intersection.is_empty() {
                                errors.push((i, intersection));
                            }
                            (set.union(&predict_sets[i]).copied().collect(), errors)
                        },
                    )
                    .1
            })
            .collect()
    }
}

fn get_grammar() -> Grammar<'static> {
    Grammar::parse(include_str!("../grammar.txt")).expect("grammar is not valid")
}

#[test]
fn is_predictive() {
    let grammar = get_grammar();
    let non_predictive = grammar.non_predictive();

    if !non_predictive.is_empty() {
        eprintln!("--- rules ---");
        for (i, rule) in grammar.rules.iter().enumerate() {
            eprint!("{i}: {} ->", rule.nonterminal.0);
            for p in &rule.productions {
                match p {
                    ProductionItem::Terminal(Terminal::Literal(s))
                    | ProductionItem::Terminal(Terminal::Named(s))
                    | ProductionItem::Nonterminal(Nonterminal(s)) => eprint!(" {s}"),
                }
            }
            eprintln!();
        }

        panic!("Grammar is non-predictive: {non_predictive:?}");
    }
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

    let _ = grammar.predict_sets();
}
