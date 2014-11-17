use std::collections::{HashMap, HashSet};
use {Grammar, Node, RuleItem, Sym, Chr, RulePos};
use show::WithGrammar;

fn fill_up_state(state: &mut Vec<RulePos>, grammar: &Grammar) {
	let mut idx = 0;
	while idx < state.len() {	// we want to append to the array as we go, so no iterator :(
		let RulePos(r, pos) = state[idx];
		idx += 1;
		let rule = &grammar.rules[r];
		if rule.seq.len() > pos {
			match rule.seq[pos] {
				Chr(_) => {},
				Sym(ref s) => {
					match grammar.nterms.get(s) {
						Some(ref sym) => {
							let (start, end) = sym.rules;
							for i in range(start,end) {
								if !state.contains(&RulePos(i, 0u)) {
									state.push(RulePos(i, 0u));
								}
							}
						}
						None => {
							match grammar.terms.get(s) {
								Some(_) => {},
								None => panic!("rule {} ({}) refers to unknown symbol {}",
										r, rule, s)
							}
						}
					}
				}
			}
		}
	}
}

fn derive_node(grammar: &Grammar, pos: uint, item: RuleItem, nodes: &mut Vec<Node>, log_level: uint) {
	if log_level > 2 {
		print!("deriving by {}: ", item);
	}
	let mut newstate: Vec<RulePos> = (*nodes)[pos].state.iter().filter(|&&RulePos(rule, p): &&RulePos| {
			let r = &grammar.rules[rule];
			if r.seq.len() <= p {
				return false;
			}
			r.seq[p] == item
		}).map(|&RulePos(rule, p): &RulePos| RulePos(rule, p + 1)).collect();
	fill_up_state(&mut newstate, grammar);
	if nodes.iter().any(|nod: &Node| {
		if nod.state.len() != newstate.len() {
			return false;
		}
		for state in nod.state.iter() {
			if !newstate.contains(state) {
				return false;
			}
		}
		true
	}) {
		return;
	}
	let idx = nodes.len();
	nodes[pos].shifts.insert(item, idx);
	if log_level > 2 {
		println!("{}", newstate);
	}
	nodes.push(Node {
		state: newstate,
		shifts: HashMap::new(),
		reduce: None
	});
}

pub fn create_nodes(grammar: &Grammar, log_level: uint) -> Vec<Node> {
	let mut nodes: Vec<Node> = Vec::new();
	let mut st = vec![RulePos(0,0)];
	fill_up_state(&mut st, grammar);
	nodes.push(Node {
		state: st,
		shifts: HashMap::new(),
		reduce: None
	});
	let start = grammar.rules[0].seq[0].clone();
	derive_node(grammar, 0, start, &mut nodes, log_level);	// enforce the accept state always being node #1
	let mut pos = 0u;	// can't use iterator here because we write to the vector
	loop {
		let mut shifts: HashSet<RuleItem> = HashSet::new();
		{
		let node = &mut nodes[pos];
		if log_level > 1 {
			println!("Node {}: {}", pos, node.with_grammar(grammar));
		}
		for &RulePos(rule, p) in node.state.iter() {
			let r = &grammar.rules[rule];
			if r.seq.len() > p {
				shifts.insert(r.seq[p].clone());
			} else {
				match node.reduce {
					None => { node.reduce = Some(rule); }
					Some(x) => panic!("Reduce-Reduce conflict in node {} ({}) between rule {} ({}) and {} ({})",
						pos, node, x, grammar.rules[x], rule, r)
				}
			}
		}
		if log_level > 1 {
			println!("-> shifts: {}", shifts);
			match node.reduce {
				None => {},
				Some(x) => println!("-> reduce: {} ({})", x, grammar.rules[x])
			}
		}
		}
		for item in shifts.into_iter() {
			derive_node(grammar, pos, item, &mut nodes, log_level);
		}
		pos += 1;
		if pos == nodes.len() {
			return nodes;
		}
	}
}