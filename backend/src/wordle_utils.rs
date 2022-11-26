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
        indices: [Condition; 5],
    }

    pub enum Condition {
        NotAnywhere,
        NotHere,
        Here,
        CouldBeHere,
    }

    impl Letter {
        pub fn new(value: char, indices: [Condition; 5]) -> Self {
            Letter { value, indices }
        }
    }

    pub fn get_words_with_matching_letters(
        all_possible_words: &Vec<String>,
        guessed_letter_vector: &Vec<Letter>,
    ) -> Vec<String> {
        let mut all_possible_words = all_possible_words.clone();
        let mut protected_indices: Vec<i8> = vec![];
        let mut all_indices_not_here = true;

        for guess_letter in guessed_letter_vector {
            for (i, char_value) in guess_letter.indices.iter().enumerate() {
                match char_value {
                    Condition::NotHere => {
                        // check if every index is NotHere
                        // if that is the case, remove words from consideration that contain the char in question
                        for condition in &guess_letter.indices {
                            match condition {
                                Condition::NotHere => (), // do nothing as all_indices_not_here is already set to true
                                _ => all_indices_not_here = false,
                            }
                        }

                        if (all_indices_not_here) {
                            all_possible_words =
                                get_filtered_words(&all_possible_words, vec![guess_letter.value]);
                        }
                        // result must not contain this letter at this specific index
                        all_possible_words = get_words_with_not_this_char_at_this_index(
                            &all_possible_words,
                            i,
                            guess_letter.value,
                        );
                    }
                    Condition::Here => {
                        protected_indices.push(i.clone().try_into().unwrap());
                        // result must contain this letter at this specific index
                        // prevent this index from being considered for any other match
                        all_possible_words = get_words_with_this_char_at_this_index(
                            &all_possible_words,
                            i,
                            guess_letter.value,
                        );
                    }
                    Condition::CouldBeHere => (), // result contains this letter at an unknown index
                    _ => (),
                }
            }
        }
        all_possible_words
    }

    pub fn get_words_with_this_char_at_this_index(
        all_possile_words: &Vec<String>,
        target_index: usize,
        target_char: char,
    ) -> Vec<String> {
        let mut filtered_words = all_possile_words.clone();
        filtered_words = filtered_words
            .into_iter()
            .filter(|word| {
                word.chars().collect::<Vec<char>>()[target_index].eq_ignore_ascii_case(&target_char)
            })
            .collect();
        filtered_words
    }

    pub fn get_words_with_not_this_char_at_this_index(
        all_possile_words: &Vec<String>,
        target_index: usize,
        target_char: char,
    ) -> Vec<String> {
        let mut filtered_words = all_possile_words.clone();
        filtered_words = filtered_words
            .into_iter()
            .filter(|word| {
                !word.chars().collect::<Vec<char>>()[target_index]
                    .eq_ignore_ascii_case(&target_char)
            })
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
        let char_exclusion_list = vec!['m', 'a', 'd', 'l', 'y'];
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
        let words: Vec<String> = vec![
            "mouth".to_string(),
            "amber".to_string(),
            "bombs".to_string(),
            "month".to_string(),
            "mangy".to_string(),
        ];
        let letters: Vec<Letter> = vec![
            Letter::new(
                'm',
                [
                    Condition::CouldBeHere,
                    Condition::NotHere,
                    Condition::CouldBeHere,
                    Condition::CouldBeHere,
                    Condition::CouldBeHere,
                ],
            ),
            Letter::new(
                'o',
                [
                    Condition::CouldBeHere,
                    Condition::Here,
                    Condition::CouldBeHere,
                    Condition::CouldBeHere,
                    Condition::CouldBeHere,
                ],
            ),
            Letter::new(
                'b',
                [
                    Condition::NotAnywhere,
                    Condition::CouldBeHere,
                    Condition::CouldBeHere,
                    Condition::CouldBeHere,
                    Condition::CouldBeHere,
                ],
            ),
        ];
        let expected = vec!["mouth".to_string(), "month".to_string()];
        let actual = get_words_with_matching_letters(&words, &letters);
        assert_eq!(expected, actual);
    }

    #[test]
    fn filter_words_by_index_and_value_test() {
        let words: Vec<String> = vec![
            "mouth".to_string(),
            "amber".to_string(),
            "tubes".to_string(),
            "scoby".to_string(),
            "death".to_string(),
        ];
        let expected = vec!["amber".to_string(), "tubes".to_string()];
        let actual = get_words_with_this_char_at_this_index(&words, 2, 'b');
        assert_eq!(expected, actual);
    }

    #[test]
    fn get_words_with_not_this_char_at_this_index_test() {
        let words: Vec<String> = vec![
            "mouth".to_string(),
            "amber".to_string(),
            "tubes".to_string(),
            "scoby".to_string(),
            "death".to_string(),
        ];
        let expected = vec![
            "mouth".to_string(),
            "scoby".to_string(),
            "death".to_string(),
        ];
        let actual = get_words_with_not_this_char_at_this_index(&words, 2, 'b');
        assert_eq!(expected, actual);
    }

    #[test]
    fn mommy_test() {
        // before we eliminate any words
        // find all (if any) known "HERE" indices pairs
        // and protect them from elimination and remove them from
        // consideration of other letters

        let words: Vec<String> = vec![
            "mouth".to_string(), // this is the "correct" word
            "amber".to_string(),
            "bombs".to_string(),
            "month".to_string(),
            "mangy".to_string(),
            "mommy".to_string(),
            "mamma".to_string(),
            "manna".to_string(),
            "mimby".to_string(),
        ];
        let letters: Vec<Letter> = vec![
            Letter::new(
                'm',
                [
                    Condition::Here,
                    Condition::CouldBeHere,
                    Condition::CouldBeHere,
                    Condition::CouldBeHere,
                    Condition::CouldBeHere,
                ],
            ),
            Letter::new(
                'a',
                [
                    Condition::NotHere,
                    Condition::NotHere,
                    Condition::NotHere,
                    Condition::NotHere,
                    Condition::NotHere,
                ],
            ),
            Letter::new(
                'd',
                [
                    Condition::NotHere,
                    Condition::NotHere,
                    Condition::NotHere,
                    Condition::NotHere,
                    Condition::NotHere,
                ],
            ),
            Letter::new(
                'a',
                [
                    Condition::NotHere,
                    Condition::NotHere,
                    Condition::NotHere,
                    Condition::NotHere,
                    Condition::NotHere,
                ],
            ),
            Letter::new(
                'm',
                [
                    Condition::CouldBeHere, // protected index
                    Condition::CouldBeHere,
                    Condition::CouldBeHere,
                    Condition::CouldBeHere,
                    Condition::NotHere,
                ],
            ),
        ];
        let expected = vec!["mommy".to_string(), "mimby".to_string()];
        let actual = get_words_with_matching_letters(&words, &letters);
        assert_eq!(expected, actual);
    }
}
