use std::collections::HashSet;
use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let scanners: Vec<Vec<[i64; 3]>> = args
        .split(|x| x.len() == 0)
        .map(|x| {
            x[1..]
                .iter()
                .map(|x| {
                    let mut split = x.split(',');
                    [
                        split.next().unwrap().parse().unwrap(),
                        split.next().unwrap().parse().unwrap(),
                        split.next().unwrap().parse().unwrap(),
                    ]
                })
                .collect()
        })
        .collect();

    const MAP: [((usize, i64), (usize, i64), (usize, i64)); 24] = [
        ((0, 1), (1, 1), (2, 1)),
        ((0, 1), (2, -1), (1, 1)),
        ((0, 1), (1, -1), (2, -1)),
        ((0, 1), (2, 1), (1, -1)),
        ((1, 1), (2, 1), (0, 1)),
        ((1, 1), (0, -1), (2, 1)),
        ((1, 1), (2, -1), (0, -1)),
        ((1, 1), (0, 1), (2, -1)),
        ((2, 1), (0, 1), (1, 1)),
        ((2, 1), (1, -1), (0, 1)),
        ((2, 1), (0, -1), (1, -1)),
        ((2, 1), (1, 1), (0, -1)),
        ((0, -1), (2, -1), (1, -1)),
        ((0, -1), (1, 1), (2, -1)),
        ((0, -1), (2, 1), (1, 1)),
        ((0, -1), (1, -1), (2, 1)),
        ((1, -1), (0, -1), (2, -1)),
        ((1, -1), (2, 1), (0, -1)),
        ((1, -1), (0, 1), (2, 1)),
        ((1, -1), (2, -1), (0, 1)),
        ((2, -1), (1, -1), (0, -1)),
        ((2, -1), (0, 1), (1, -1)),
        ((2, -1), (1, 1), (0, 1)),
        ((2, -1), (0, -1), (1, 1)),
    ];

    let mut scan_pos: Vec<Option<[i64; 3]>> = vec![None; scanners.len()];
    scan_pos[0] = Some([0; 3]);

    let mut beacons: HashSet<[i64; 3]> = scanners[0].clone().into_iter().collect();

    while scan_pos.contains(&None) {
        for j in scan_pos
            .clone()
            .iter()
            .enumerate()
            .filter(|x| x.1.is_none())
            .map(|x| x.0)
        {
            println!("scanner: {}", j);
            for perm in MAP.iter() {
                let n_scanner_j: HashSet<[i64; 3]> = scanners[j]
                    .iter()
                    .map(|x| {
                        [
                            x[perm.0 .0] * perm.0 .1,
                            x[perm.1 .0] * perm.1 .1,
                            x[perm.2 .0] * perm.2 .1,
                        ]
                    })
                    .collect();
                let mut found = false;
                for ci in beacons.clone().iter() {
                    for cj in n_scanner_j.iter() {
                        let dif = [ci[0] - cj[0], ci[1] - cj[1], ci[2] - cj[2]];
                        let n_scanner_j: HashSet<[i64; 3]> = n_scanner_j
                            .iter()
                            .map(|x| [x[0] + dif[0], x[1] + dif[1], x[2] + dif[2]])
                            .collect();
                        if beacons.intersection(&n_scanner_j).count() >= 12 {
                            println!("{:?}", (&scan_pos, j));
                            scan_pos[j] = Some([dif[0], dif[1], dif[2]]);
                            beacons.extend(n_scanner_j.iter());
                            found = true;
                            break;
                        }
                        if found {
                            break;
                        }
                    }
                    if found {
                        break;
                    }
                }
            }
        }
    }

    let scan_pos: Vec<[i64; 3]> = scan_pos.into_iter().flatten().collect();
    println!("{:?}", scan_pos);

    let mut max = 0;
    for i in 0..scan_pos.len() {
        for j in (i + 1)..scan_pos.len() {
            let dist = (scan_pos[i][0] - scan_pos[j][0]).abs()
                + (scan_pos[i][1] - scan_pos[j][1]).abs()
                + (scan_pos[i][2] - scan_pos[j][2]).abs();
            max = std::cmp::max(max, dist);
        }
    }

    println!("{}", max);
}
