use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let mut squids: Vec<Vec<u8>> = args
        .iter()
        .map(|x| {
            x.chars()
                .map(|x| x.to_digit(10).unwrap() as u8)
                .collect::<Vec<u8>>()
        })
        .collect();

    let neigbours: [(i8, i8); 8] = [
        (-1, -1),
        (0, -1),
        (1, -1),
        (-1, 0),
        (1, 0),
        (-1, 1),
        (0, 1),
        (1, 1),
    ];

    let mut step = 0;
    loop {
        let mut flashed = vec![vec![false; 10]; 10];
        for i in 0..10 {
            for j in 0..10 {
                squids[i][j] += 1;
            }
        }
        let mut found_flash = true;
        while found_flash {
            found_flash = false;
            for i in 0..10i8 {
                for j in 0..10i8 {
                    let (m, w) = (i as usize, j as usize);
                    if squids[m][w] > 9 {
                        found_flash = true;
                        squids[m][w] = 0;
                        flashed[m][w] = true;
                        for n in neigbours {
                            if i + n.0 >= 0 && i + n.0 <= 9 && j + n.1 >= 0 && j + n.1 <= 9 {
                                let (k, l) = ((i + n.0) as usize, (j + n.1) as usize);
                                if !flashed[k][l] {
                                    squids[k][l] += 1;
                                }
                            }
                        }
                    }
                }
            }
        }
        step += 1;
        if squids.iter().flatten().find(|&&x| x > 0).is_none() {
            break;
        }
    }

    println!("{}", step);
}
