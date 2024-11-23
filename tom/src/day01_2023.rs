use std::fs;

pub fn main() {
    let part1 = fs::read_to_string("data/2023_day01.txt")
        .expect("Contents from file")
        .lines()
        .map(|line| line.chars().filter_map(|x| x.to_digit(10)).collect())
        .collect::<Vec<Vec<u32>>>()
        .into_iter()
        .fold(0, |acc: u32, val: Vec<u32>| {
            acc + format!("{}{}", val.first().unwrap(), val.last().unwrap())
                .parse::<u32>()
                .unwrap()
        });
    println!("Part 1 -> {}", part1);
}
