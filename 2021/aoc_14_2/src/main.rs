use std::collections::HashMap;
use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let mut template = HashMap::new();
    for e in args[0].chars().zip(args[0].chars().skip(1)) {
        *template.entry(e).or_insert(0u64) += 1;
    }

    let rules: Vec<((char, char), char)> = args[2..]
        .iter()
        .map(|x| {
            let mut s = x.split(" -> ");
            let mut f = s.next().unwrap().chars();
            (
                (f.next().unwrap(), f.next().unwrap()),
                s.next().unwrap().chars().next().unwrap(),
            )
        })
        .collect();

    for _ in 0..40 {
        let mut new_template = HashMap::new();
        for e in template.iter() {
            if let Some((_, rule)) = rules.iter().find(|&x| x.0 == *e.0) {
                *new_template.entry((e.0 .0, *rule)).or_insert(0u64) += *e.1;
                *new_template.entry((*rule, e.0 .1)).or_insert(0u64) += *e.1;
            } else {
                *new_template.entry(*e.0).or_insert(0u64) += *e.1;
            }
        }
        template = new_template;
    }

    let mut map = HashMap::new();
    for e in template.iter() {
        *map.entry(e.0 .0).or_insert(0u64) += e.1;
    }
    *map.entry(args[0].chars().last().unwrap()).or_insert(0u64) += 1;

    println!(
        "{}",
        map.clone().into_values().max().unwrap() - map.into_values().min().unwrap()
    );
}
