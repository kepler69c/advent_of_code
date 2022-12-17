use std::io::BufRead;

#[derive(Debug)]
enum Inst {
    Addx(i32),
    WAddx,
    Noop,
}

fn main() {
    let args = std::io::stdin().lock().lines().flatten();
    let inst = args.flat_map(|x| {
        if &x == "noop" {
            vec![Inst::Noop].into_iter()
        } else {
            vec![
                Inst::WAddx,
                Inst::Addx(x.split_once(' ').unwrap().1.parse().unwrap()),
            ]
            .into_iter()
        }
    });
    let mut int_cycle = 19;
    let (_, ss) = inst
        .enumerate()
        .fold((1, Vec::new()), |(reg, mut ss), (i, inst)| {
            if i == int_cycle && int_cycle <= 220 {
                ss.push(reg * (i + 1) as i32);
                int_cycle += 40;
            }
            let reg = match inst {
                Inst::Noop | Inst::WAddx => reg,
                Inst::Addx(v) => reg + v,
            };
            (reg, ss)
        });
    println!("{}", ss.iter().sum::<i32>());
}
