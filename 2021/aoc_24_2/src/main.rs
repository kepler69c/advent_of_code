use std::io::BufRead;

fn getv(x: &str) -> Option<usize> {
    match x {
        "w" => Some(0),
        "x" => Some(1),
        "y" => Some(2),
        "z" => Some(3),
        _ => None,
    }
}

fn apply_op(op: &String, wxyz: &mut [i64; 4], inp: &mut Vec<i64>) {
    let split: Vec<&str> = op.split(' ').collect();
    let b = if let Some(v) = split.get(2) {
        if let Some(b) = getv(v) {
            Some(wxyz[b])
        } else {
            Some(v.parse().unwrap())
        }
    } else {
        None
    };
    let a = &mut wxyz[getv(split[1]).unwrap()];
    match split[0] {
        "inp" => {
            *a = inp.drain(0..1).next().unwrap();
        }
        "add" => {
            *a += b.unwrap();
        }
        "mul" => {
            *a *= b.unwrap();
        }
        "div" => {
            *a /= b.unwrap();
        }
        "mod" => {
            *a %= b.unwrap();
        }
        "eql" => {
            *a = (*a == b.unwrap()) as i64;
        }
        _ => (),
    }
}

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let mut min = i64::MAX;
    let list: Vec<i64> = vec![16181111641521];
    for i in list {
        let mut inp = Vec::new();
        let mut ip = i;
        for _ in 0..14 {
            inp.push(ip % 10);
            ip /= 10;
        }
        if inp.contains(&0) {
            continue;
        }
        inp = inp.into_iter().rev().collect();

        let mut wxyz = [0; 4];

        for line in args.iter() {
            apply_op(line, &mut wxyz, &mut inp);
            //println!("{}    \t => {:?}", line, wxyz);
        }
        min = std::cmp::min(min, wxyz[3]);
        println!("{} - {:?}", i, wxyz);
    }
}
