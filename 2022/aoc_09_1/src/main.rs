use std::{collections::HashSet, io::BufRead};

fn main() {
    let args = std::io::stdin().lock().lines().flatten();
    let moves = args.map(|x| {
        let (dir_s, n_s) = x.split_once(' ').unwrap();
        let dir = match dir_s.chars().next().unwrap() {
            'U' => (1, 0),
            'D' => (-1, 0),
            'R' => (0, 1),
            'L' => (0, -1),
            _ => panic!(),
        };
        (dir, n_s.parse::<u32>().unwrap())
    });

    let mut h_pos = (0, 0);
    let mut tail_trail = HashSet::new();
    let mut t_rel: (i32, i32) = (0, 0);

    moves.for_each(|(dir, n)| {
        (0..n).for_each(|_| {
            h_pos = (h_pos.0 + dir.0, h_pos.1 + dir.1);
            t_rel = (t_rel.0 - dir.0, t_rel.1 - dir.1);
            if t_rel.0.abs() == 2 {
                t_rel.0 /= 2;
                if t_rel.1.abs() == 1 {
                    t_rel.1 = 0;
                }
            } else if t_rel.1.abs() == 2 {
                t_rel.1 /= 2;
                if t_rel.0.abs() == 1 {
                    t_rel.0 = 0;
                }
            }
            tail_trail.insert((h_pos.0 + t_rel.0, h_pos.1 + t_rel.1));
        });
    });

    println!("{}", tail_trail.len());
}
