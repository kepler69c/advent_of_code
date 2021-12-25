use std::cmp::{Ordering, Reverse};
use std::collections::{BTreeSet, BinaryHeap};
use std::io::BufRead;

#[derive(Eq, Debug)]
struct AStarNode {
    x: u32,
    y: u32,
    cost: u32,
    heuristics: u32,
}

impl PartialEq for AStarNode {
    fn eq(&self, other: &Self) -> bool {
        self.heuristics == other.heuristics
    }
}

impl PartialOrd for AStarNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}

impl Ord for AStarNode {
    fn cmp(&self, other: &Self) -> Ordering {
        self.heuristics.cmp(&other.heuristics)
    }
}

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let risk_map: Vec<Vec<u32>> = args
        .iter()
        .map(|x| x.chars().map(|x| x.to_digit(10).unwrap()).collect())
        .collect();
    let dim = (risk_map.len(), risk_map[0].len());

    let mut whole_risk_map = vec![vec![0; dim.0 * 5]; dim.1 * 5];
    for (i, line) in risk_map.iter().enumerate() {
        for (j, e) in line.iter().enumerate() {
            for ii in 0..5 {
                for jj in 0..5 {
                    whole_risk_map[dim.0 * ii + i][dim.1 * jj + j] =
                        (e + ii as u32 + jj as u32 - 1) % 9 + 1;
                }
            }
        }
    }
    let w_dim = (risk_map.len() * 5, risk_map[0].len() * 5);
    let objective = (w_dim.0 as u32 - 1, w_dim.1 as u32 - 1);

    let mut closed = BTreeSet::new();
    let mut open = BinaryHeap::new();
    open.push(Reverse(AStarNode {
        x: 0,
        y: 0,
        cost: 0,
        heuristics: 0,
    }));

    while open.len() > 0 {
        let u = open.pop().unwrap().0;
        if u.x == objective.1 && u.y == objective.0 {
            println!(
                "{}",
                u.cost + whole_risk_map[objective.0 as usize][objective.1 as usize]
                    - whole_risk_map[0][0]
            );
            return;
        }
        // neighbours list
        let n = [
            if u.x > 0 { Some((u.y, u.x - 1)) } else { None },
            if u.x < w_dim.1 as u32 - 1 {
                Some((u.y, u.x + 1))
            } else {
                None
            },
            if u.y > 0 { Some((u.y - 1, u.x)) } else { None },
            if u.y < w_dim.0 as u32 - 1 {
                Some((u.y + 1, u.x))
            } else {
                None
            },
        ];
        for &v in n.iter().flatten() {
            if !(closed.contains(&v)
                || open
                    .iter()
                    .any(|x| (x.0.y, x.0.x) == v && x.0.cost <= u.cost))
            {
                let (y, x) = v;
                let cost = u.cost + whole_risk_map[u.y as usize][u.x as usize];
                open.push(Reverse(AStarNode {
                    x,
                    y,
                    cost,
                    heuristics: cost
                        + (i64::abs(x as i64 - objective.1 as i64) as u32
                            + i64::abs(y as i64 - objective.0 as i64) as u32),
                }));
            }
        }
        closed.insert((u.y, u.x));
    }
}
