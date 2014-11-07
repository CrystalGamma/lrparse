use std::collections::{HashMap, HashSet};
use {Grammar, Node, RuleItem, Sym, Chr};

fn fill_up_state(state: &mut Vec<(uint, uint)>, grammar: &Grammar) {
	let mut idx = 0;
	while idx < state.len() {	// we want to append to the array as we go, so no iterator :(
		let (r, pos) = state[idx];
		idx += 1;
		let rule = &grammar.rules[r];
		if rule.seq.len() > pos {
			match rule.seq[pos] {
				Chr(_) => {},
				Sym(ref s) => {
					match grammar.nterms.find(s) {
						Some(ref sym) => {
							let (start, end) = sym.rules;
							for i in range(start,end) {
								if !state.contains(&(i, 0u)) {
									state.push((i, 0u));
								}
							}
						}
						None => {
							match grammar.terms.find(s) {
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

fn derive_node(grammar: &Grammar, pos: uint, item: RuleItem, nodes: &mut Vec<Node>) {
	print!("deriving by {}: ", item);
	let mut newstate: Vec<(uint,uint)> = (*nodes)[pos].state.iter().filter(|&&(rule, p): &&(uint, uint)| {
			let r = &grammar.rules[rule];
			if r.seq.len() <= p {
				return false;
			}
			r.seq[p] == item
		}).map(|&(rule, p): &(uint, uint)| (rule, p + 1)).collect();
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
	println!("{}", newstate);
	nodes.push(Node {
		state: newstate,
		shifts: HashMap::new(),
		reduce: None
	});
}

pub fn create_nodes(grammar: &Grammar) -> Vec<Node> {
	let mut nodes: Vec<Node> = Vec::new();
	let mut st = vec![(0,0)];
	fill_up_state(&mut st, grammar);
	nodes.push(Node {
		state: st,
		shifts: HashMap::new(),
		reduce: None
	});
	let start = grammar.rules[0].seq[0].clone();
	derive_node(grammar, 0, start, &mut nodes);	// enforce the accept state always being node #1
	let mut pos = 0u;	// can't use iterator here because we write to the vector
	loop {
		let mut shifts: HashSet<RuleItem> = HashSet::new();
		{
		let node = &mut nodes[pos];
		println!("Node {}: {}", pos, node);
		for &(rule, p) in node.state.iter() {
			let r = &grammar.rules[rule];
			if r.seq.len() > p {
				println!("rule {}, shift by {}", rule, r.seq[p]);
				shifts.insert(r.seq[p].clone());
			} else {
				match node.reduce {
					None => { node.reduce = Some(rule); }
					Some(x) => panic!("Reduce-Reduce conflict in node {} ({}) between rule {} ({}) and {} ({})",
						pos, node, x, grammar.rules[x], rule, r)
				}
			}
		}
		println!("-> shifts: {}", shifts);
		match node.reduce {
			None => {},
			Some(x) => println!("-> reduce: {} ({})", x, grammar.rules[x])
		}
		}
		for item in shifts.into_iter() {
			derive_node(grammar, pos, item, &mut nodes);
		}
		pos += 1;
		if pos == nodes.len() {
			return nodes;
		}
	}
}