let rec match_line = (pattern, text): bool =>
  switch (String.length(pattern) > String.length(text)) {
  | false =>
    let first_letter = pattern.[0];

    let first_letter_index =
      switch (String.index(text, first_letter)) {
      | exception Not_found => None
      | x => Some(x)
      };

    switch (first_letter_index) {
    | None => false
    | Some(first_letter_index) =>
      switch (
        String.length(text) >= String.length(pattern) + first_letter_index
      ) {
      | true =>
        let possible_match =
          String.sub(text, first_letter_index, String.length(pattern));

        switch (String.equal(possible_match, pattern)) {
        | false =>
          let new_text =
            String.sub(
              text,
              first_letter_index + 1,
              String.length(text) - first_letter_index - 1,
            );
          match_line(pattern, new_text);
        | _ => true
        };
      | _ => false
      }
    };

  | true => false
  };

let lines = ["hack", "night a", "a", "reason", "sthlm"];

let grep = pattern => List.filter(match_line(pattern));

let results = grep("k", lines);

List.iter(x => print_string(x ++ "\n"), results);

/* if (Array.length(Sys.argv) == 3) { */
/*   let grep_result = grep(Sys.argv[1], Sys.argv[2]); */

/*   let output = */
/*     switch (grep_result) { */
/*     | false => */
/*       Pastel.( */
/*         <Pastel bold=true color=Red> */
/*           <Pastel italic=true underline=true> "No match found!" </Pastel> */
/*         </Pastel> */
/*       ) */
/*     | true => */
/*       Pastel.( */
/*         <Pastel bold=true color=Green> */
/*           <Pastel italic=true underline=true> "Found a match!" </Pastel> */
/*         </Pastel> */
/*       ) */
/*     }; */
/*   print_endline(output); */
/* } else { */
/*   print_string("Used: regrep \"pattern\" \"text\"\n"); */
/* }; */
