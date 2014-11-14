use std::sync::Arc;
use std::collections::{HashMap, HashSet};

use nester::{Token, Tok, Tree, Identifier, Char, Other, Arrow, CodeReference};
use {Grammar, RuleItem, Sym, Chr, Rule, NTerm};

fn parse_rules(tree: &[Token], nterm: &Arc<String>, grammar: &mut Grammar) -> (uint, uint) {
	let mut pos = 0;
	let start = grammar.rules.len();
	loop {
		let mut seq: Vec<RuleItem> = Vec::new();
		loop {
			match tree[pos].content {
				Tok(Identifier(ref s)) => {
					seq.push(Sym(s.clone()));
					pos += 1;
				},
				Tok(Char(c)) => {
					seq.push(Chr(c));
					pos += 1;
				},
				Tree('}', _) => {
					grammar.rules.push(Rule {
						seq: seq,
						code: tree[pos].clone(),
						nterm: nterm.clone()
					});
					pos += 1;
					break;
				},
				Tok(Arrow) => { 
					pos += 1;
				}
				_ => panic!()
			}
		}
		println!("rule {}: {}", grammar.rules.len() - 1, grammar.rules.last());
		if pos != tree.len() {
			match tree[pos].content {
				Tok(Other(',')) => { pos += 1; },
				_ => {}
			}
		}
		if pos == tree.len() {
			return (start, grammar.rules.len());
		}
	}
}

pub fn parse_grammar(tree: &[Token]) -> Grammar {
	let mut grammar = Grammar {
		prelude: Vec::new(),
		terms: HashMap::new(),
		chars: HashSet::new(),
		nterms: HashMap::new(),
		rules: Vec::new(),
		errors: HashMap::new()
	};
	grammar.nterms.insert("Accept_".to_string(), NTerm {
		type_: Vec::new(),
		rules: (0u, 1u)
	});
	grammar.rules.push(Rule {
		seq: Vec::new(),
		code: Token {
			content: Tree('}', Vec::new()),	// TODO
			ref_: CodeReference::internal()
		},
		nterm: Arc::new("Accept_".to_string())
	});
	let mut pos = 0;
	let mut startsymbol = true;
	loop {
		match match tree[pos].content {
			Tok(ref x) => x,
			Tree(..) => panic!()
		} {
			&Other('#') => {
				pos += 1;
				match tree[pos].content {
					Tree('}', ref t @ _) => {
						grammar.prelude.push_all(t.as_slice());
						pos += 1;
					},
					Tok(Identifier(ref id)) => {
						pos += 1;
						match tree[pos].content {
							Tree(')', ref t @ _) => if !grammar.terms.insert(id.clone(), t.clone()) {
								panic!() 
							} else {
								pos += 1;
							},
							_ => if !grammar.terms.insert(id.clone(), Vec::new()) {
								panic!();
							}
						}
					}
					_ => panic!()
				}
			},
			&Identifier(ref name) => {
				pos += 1;
				let arc = Arc::new(name.clone());
				let type_ = match tree[pos].content {
					Tree('}', _) => Vec::new(),
					Tree(')',  ref t @ _) => {
						pos +=1;
						t.clone()
					},
					_ => panic!()
				};
				match tree[pos].content {
					Tree('}', ref t @ _) => {
						pos += 1;
						if startsymbol {
							match grammar.nterms.find_mut(&"Accept_".to_string()) {
								Some(x) => { x.type_ = type_.clone(); }
								None => panic!()
							}
							grammar.rules[0].seq = vec![Sym(name.clone())];
							startsymbol = false;
						}
						let nterm = NTerm {
							type_: type_,
							rules: parse_rules(t.as_slice(), &arc, &mut grammar)
						};
						grammar.nterms.insert(name.clone(), nterm);
					},
					_ => panic!()
				}
			},
			&Other('!') => {
				pos += 1;
				match tree[pos].content {
					Tok(Identifier(ref s)) => {
						pos += 1;
						let name = s.clone();
						let typ = match tree[pos].content {
							Tree(')', ref t) => {
								pos += 1;
								t.clone()
							},
							_ => Vec::new()
						};
						grammar.errors.insert(name, typ);
					},
					_ => panic!("unexpected token {} at {}", tree[pos].content, tree[pos].ref_)
				}
			},
			_ => panic!("unexpected token {} at {}", tree[pos].content, tree[pos].ref_)
		}
		if pos != tree.len() {
			match tree[pos].content {
				Tok(Other(',')) => { pos += 1; },
				_ => {}
			}
		}
		if pos == tree.len() {
			break;
		}
	}
	grammar
}