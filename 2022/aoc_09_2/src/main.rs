use std::{collections::HashSet, io::BufRead};

fn main() {
    let args = std::io::stdin().lock().lines().flatten();
    let moves = args.map(|x| {
        let (dir_s, n_s) = x.split_once(' ').unwrap();
        let dir = match dir_s.chars().next().unwrap() {
            'U' => (-1, 0),
            'D' => (1, 0),
            'R' => (0, 1),
            'L' => (0, -1),
            _ => panic!(),
        };
        (dir, n_s.parse::<u32>().unwrap())
    });

    let mut h_pos: (i32, i32) = (0, 0);
    let mut t_pos: Vec<(i32, i32)> = vec![(0, 0); 9];
    let mut tail_trail: HashSet<(i32, i32)> = HashSet::new();

    moves.for_each(|(dir, n)| {
        (0..n).for_each(|_| {
            h_pos = (h_pos.0 + dir.0, h_pos.1 + dir.1);

            (0..9).for_each(|i| {
                let (h, t) = (if i == 0 { h_pos } else { t_pos[i - 1] }, &mut t_pos[i]);

                let mut t_rel = (t.0 - h.0, t.1 - h.1);
                if t_rel.0.abs() == 2 && t_rel.1.abs() == 2 {
                    t_rel.0 /= 2;
                    t_rel.1 /= 2;
                } else if t_rel.0.abs() == 2 {
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

                *t = (h.0 + t_rel.0, h.1 + t_rel.1);
            });

            tail_trail.insert(*t_pos.last().unwrap());
        });
    });

    println!("{}", tail_trail.len());
}
