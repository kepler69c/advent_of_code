use std::collections::HashMap;
use std::collections::HashSet;
use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let risk_map: Vec<Vec<u32>> = args
        .iter()
        .map(|x| x.chars().map(|x| x.to_digit(10).unwrap()).collect())
        .collect();

    let dim = (risk_map[0].len(), risk_map.len());
    let mut dist = HashMap::new();
    let mut prev = HashMap::new();
    let mut val = HashMap::new();
    let mut rem = HashSet::new();

    for (j, line) in risk_map.iter().enumerate() {
        for (i, v) in line.iter().enumerate() {
            dist.insert((i, j), u32::MAX);
            prev.insert((i, j), None);
            val.insert((i, j), v);
            rem.insert((i, j));
        }
    }
    dist.insert((0, 0), 0);

    while rem.len() > 0 {
        let (&u, &d) = dist
            .iter()
            .filter(|(k, _)| rem.contains(k))
            .min_by_key(|x| x.1)
            .unwrap();

        if u == (dim.0 - 1, dim.1 - 1) {
            let mut risk = 0;
            let mut u = Some(u);
            while let Some(u_w) = u {
                risk += *val[&u_w];
                u = prev[&u_w];
            }
            println!("{}", risk - val[&(0, 0)]);
            break;
        }

        rem.remove(&u);

        // neighbors
        let (i, j) = u;
        let mut n = Vec::new();
        if i > 0 {
            n.push((i - 1, j));
        }
        if i < dim.0 - 1 {
            n.push((i + 1, j));
        }
        if j > 0 {
            n.push((i, j - 1));
        }
        if j < dim.1 - 1 {
            n.push((i, j + 1));
        }

        for v in n.iter().filter(|x| rem.contains(x)) {
            let alt = d + val[v];
            if alt < dist[v] {
                dist.insert(*v, alt);
                prev.insert(*v, Some(u));
            }
        }
    }
}
