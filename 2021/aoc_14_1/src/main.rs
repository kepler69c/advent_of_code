use std::collections::HashMap;
use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let mut template = args[0].clone();
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

    for _ in 0..10 {
        let mut new_template = String::from("");
        for (c1, c2) in template.chars().zip(template.chars().skip(1)) {
            new_template.push(c1);
            if let Some((_, rule)) = rules.iter().find(|&x| x.0 == (c1, c2)) {
                new_template.push(*rule);
            }
        }
        new_template.push(template.chars().last().unwrap());
        template = new_template;
    }

    let mut map: HashMap<char, u64> = HashMap::new();

    for c in template.chars() {
        *map.entry(c).or_insert(0) += 1;
    }

    println!(
        "{}",
        map.clone().into_values().max().unwrap() - map.into_values().min().unwrap()
    );
}
