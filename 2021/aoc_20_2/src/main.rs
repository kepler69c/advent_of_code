use std::io::BufRead;

fn main() {
    let args: Vec<String> = std::io::stdin().lock().lines().flatten().collect();

    let algorithm: Vec<bool> = args[0].chars().map(|x| x == '#').collect();

    let source_image: Vec<Vec<bool>> = args[2..]
        .iter()
        .map(|x| x.chars().map(|x| x == '#').collect())
        .collect();

    let nba = 50;
    let mut input_image = vec![
        vec![false; source_image[0].len() + (2 * 2 * nba)];
        source_image.len() + (2 * 2 * nba)
    ];
    for i in 0..source_image.len() {
        for j in 0..source_image[0].len() {
            input_image[i + 2 * nba][j + 2 * nba] = source_image[i][j];
        }
    }

    let mut lol = false;
    for _ in 0..nba {
        let mut output_image = vec![
            vec![
                if algorithm[0] && !algorithm[511] {
                    lol = !lol;
                    lol
                } else {
                    algorithm[0]
                };
                input_image[0].len()
            ];
            input_image.len()
        ];
        for i in 1..input_image.len() - 2 {
            for j in 1..input_image[0].len() - 2 {
                let mut index = 0;
                let mut a_index = 0;
                for ii in 0..3 {
                    for jj in 0..3 {
                        let (ii, jj) = (ii as i32 - 1, jj as i32 - 1);
                        if input_image[(i as i32 + ii) as usize][(j as i32 + jj) as usize] {
                            a_index += 1 << (8 - index);
                        }
                        index += 1;
                    }
                }
                output_image[i][j] = algorithm[a_index];
            }
        }
        input_image = output_image;
    }

    println!(
        "{}",
        input_image
            .iter()
            .flatten()
            .fold(0, |acc, &x| if x { acc + 1 } else { acc })
    );
}
