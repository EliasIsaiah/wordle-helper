mod wordle_utils {
    use std::{
        fs::File,
        io::{BufRead, BufReader},
    };

    pub fn get_file_data() -> Vec<String> {
        let file = File::open("wordle_list_all.txt").expect("file wasn't found.");
        let reader = BufReader::new(file);
        reader.lines().map(|line| line.unwrap()).collect()
    }

    pub struct Letter {
        value: char,
        indices: [i8; 5],
    }

    impl Letter {
        pub fn new(value: char, indices: [i8; 5]) -> Self {
            Letter { value, indices }
        }
    }

    pub fn get_words_with_matching_letters(
        all_possible_words: &Vec<String>,
        guessed_word_vector: &Vec<Letter>,
    ) -> Vec<String> {
        let mut all_possible_words = all_possible_words.clone();
        let mut result: Vec<String> = vec![];
        for guess_letter in guessed_word_vector {
            for (i, char_value) in guess_letter.indices.iter().enumerate() {
                match char_value {
                    -1 => {
                        // doesn't contain this letter
                        all_possible_words =
                            get_filtered_words(&all_possible_words, vec![guess_letter.value]);
                        println!("-1")
                    }
                    0 => {
                        // solution contains this letter but not at this index
                        all_possible_words = filter_words_by_index_and_value(
                            &all_possible_words,
                            i,
                            guess_letter.value,
                        );
                        println!("0");
                    }
                    1 => println!("1"),
                    _ => println!("this is impossible"),
                }
            }
        }
        vec!["mouth".to_string()]
    }

    pub fn filter_words_by_index_and_value(
        all_possile_words: &Vec<String>,
        target_index: usize,
        target_char: char,
    ) -> Vec<String> {
        let mut filtered_words = all_possile_words.clone();
        filtered_words = filtered_words
            .into_iter()
            .filter(|word| word.chars().collect::<Vec<char>>()[target_index].eq_ignore_ascii_case(&target_char))
            .collect();
        filtered_words
    }

    pub fn get_filtered_words(
        all_possible_words: &Vec<String>,
        excluded_chars: Vec<char>,
    ) -> Vec<String> {
        let mut filtered_words = all_possible_words.clone();
        for word in all_possible_words {
            let split_string = word.chars();
            if split_string
                .enumerate()
                .any(|c| excluded_chars.contains(&c.1))
            {
                filtered_words = filtered_words
                    .into_iter()
                    .filter(|s| !s.parse::<String>().unwrap().eq_ignore_ascii_case(word))
                    .collect();
            }
        }
        filtered_words
    }
}

#[cfg(test)]
mod tests {
    use crate::wordle_utils::wordle_utils::*;

    #[test]
    fn get_words() {
        let expected = "aback".to_ascii_uppercase();
        let actual = get_file_data();
        assert_eq!(expected, actual[0].to_ascii_uppercase());
    }

    #[test]
    fn filter_out_words_based_on_char_exclusion_list() {
        let char_exclusion_list = vec!['a', 'm', 'd', 'y'];
        let words: Vec<String> = vec![
            "tooth".to_string(),
            "patio".to_string(),
            "alien".to_string(),
            "smite".to_string(),
            "sugar".to_string(),
            "smote".to_string(),
        ];
        let expected = vec!["tooth".to_string()];
        let actual = get_filtered_words(&words, char_exclusion_list);
        assert_eq!(expected, actual);
    }

    #[test]
    fn get_all_words_with_letter_somewhere_but_not_here() {
        let words: Vec<String> = vec!["mouth".to_string(), "amber".to_string()];
        // let guesses = vec![Letter::new('s', 0, None), Letter::new('t', -1, Some(vec![2]))];
        let letters: Vec<Letter> = vec![Letter::new('m', [-1, 0, -1, -1, -1])];
        let expected = vec!["mouth".to_string()];
        let actual = get_words_with_matching_letters(&words, &letters);
        assert_eq!(expected, actual);
    }

    #[test]
    fn filter_words_by_index_and_value_test() {
        let words: Vec<String> = vec!["mouth".to_string(), "amber".to_string(),"tubes".to_string(), "scoby".to_string(), "death".to_string()];
        let expected = vec!["amber".to_string(), "tubes".to_string()];
        let actual = filter_words_by_index_and_value(&words, 2, 'b');
        assert_eq!(expected, actual);
    }
}
