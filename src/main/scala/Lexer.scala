case class Lexer (spec: String) {

  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */
  def lex(word: String): Either[String,List[(String,String)]] = {
    var nfas = List[Nfa[Int]]() // the list of all nfas from the regexes from spec
    val tokenAndRegex = spec.split(";\n") // gets each line from spec
    var lineInSpec = 0                           // the index of each line in spec
    var index = 0
    while (index < tokenAndRegex.length) { // goes through all lines in spec
      val tar = tokenAndRegex(index).split(": ") // splits a line in the 2 components: token and regex
      if (nfas != null && nfas.nonEmpty) { // no nfa was created before
        if (tar(1) == "'\\n'") {
          nfas = Nfa.fromPrenex(Regex.toPrenex("'\n'"), tar(0), lineInSpec, nfas.head.getStates.max) :: nfas
        } else {
          nfas = Nfa.fromPrenex(Regex.toPrenex(tar(1)), tar(0), lineInSpec, nfas.head.getStates.max) :: nfas
        }
      } else { // just add one more nfa to the collection
        if (tar(1) == "'\\n'") {
          nfas = Nfa.fromPrenex(Regex.toPrenex("'\n'"), tar(0), lineInSpec, -1) :: nfas
        } else {
          nfas = Nfa.fromPrenex(Regex.toPrenex(tar(1)), tar(0), lineInSpec, -1) :: nfas
        }
      }
      lineInSpec = lineInSpec + 1
      index += 1
    }

    // builds up the final nfa from all nfas created
    val newFirstState = nfas.head.getStates.max + 1 // the new initial state
    var setOfSatesForFirst = Set[Int]() // the set of states from the new first state to the old ones
    var newAlphabet = Set[Char]()
    for (nfa <- nfas) {
      setOfSatesForFirst = setOfSatesForFirst + nfa.firstState // adds each old first state
      newAlphabet = nfa.alphabet ++ newAlphabet
    }

    var statesForFirst = Map[Char, Set[Int]]() // the transitions for the new initial state
    statesForFirst = statesForFirst + ('-' -> setOfSatesForFirst)

    // starts building the nfa with the transitions
    var transitions = Map[Int, Map[Char, Set[Int]]]()
    var finalStates = Set[Int]()
    for (nfa <- nfas) { // all the existing transitions
      transitions = nfa.transitions ++ transitions
      finalStates = finalStates + nfa.lastState
    }
    transitions = transitions + (newFirstState -> statesForFirst)

    val bigNfa = NfaFinal(newFirstState, finalStates, newAlphabet, transitions)
    val bigDfa = Dfa.fromNfa(bigNfa) // the dfa from the final nfa

    var currentLine = 0                     // the current line in the text given
    var results = List[(String,String)]()   // the list required by this function
    var currentTuple = ("","")              // the current tuple at each character
    var currentState = 0                    // the current state at each iteration
    var longestSubstringIndex = 0           // the last index of the longest substring
    var longestSubstringLen = 0             // the length of the substring
    var charIndex = 0                       // the index of the current character at each iteration
    var currentSubstring = ""               // the current substring
    var found = false                       // a boolean value for each found lexeme
    while (charIndex < word.length) { // goes through each character of the word
      if (word.charAt(charIndex).equals('\n')) { // end of line
        if (currentTuple == ("", "")) { // no lexeme found
          return Left("No viable alternative at character EOF, line " + currentLine / 2)
        }
        currentLine += 1
      }

      if (bigDfa.next(currentState, word.charAt(charIndex)) == Left(-1)) { // sink state
        if (!found) { // the dfa did not accept any character
          return Left("No viable alternative at character " + charIndex + ", line " + currentLine)
        } else {
          if (currentTuple != ("","")) { // the tuple is valid and it is added to the results
            results = results ++ (currentTuple :: Nil)
            found = false
          }
          currentState = 0 // resets all the above explained variables
          charIndex = longestSubstringIndex + 1
          currentSubstring = ""
          longestSubstringLen = 0
        }
      } else { // a state different from the sink state
        bigDfa.next(currentState, word.charAt(charIndex)) match {
          case Right(x) => currentState = x
          case Left(_) =>
        }

        var finall = false // suppose the state is not final
        for (s <- bigDfa.finalStates) {
          if (s._1 == currentState) { // it is a final state in the dfa
            finall = true
            currentSubstring = currentSubstring + word.charAt(charIndex)
            longestSubstringLen += 1
            longestSubstringIndex = charIndex

            val finalStatesFromNfa = s._2 // look for the matching token in the nfas
            for (st <- finalStatesFromNfa) {
              for (nfa <- nfas) {
                if (nfa.lastState == st) {
                  currentTuple = (currentSubstring, nfa.token) // build up the tuple
                  found = true // a lexeme was found
                }
              }
            }
            if (word.length == charIndex + 1) { // the end of word edge case
              results = results ++ (currentTuple :: Nil) // add to the results
              found = false
            }
          }
        }
        if (!finall) { // just found another character, continues
          currentSubstring = currentSubstring + word.charAt(charIndex)
          longestSubstringLen += 1
          if (word.length == charIndex + 1) { // the end of word edge case
            if (!found) return Left("No viable alternative at character EOF, line " + currentLine/2)
            else { // a tuple was found and it must be added to the results
              results = results ++ (currentTuple :: Nil)
              charIndex -= 1 // ensures that it does not go to the next char before verifying this char again
              currentState = 0
              currentSubstring = ""
              longestSubstringLen = 0
              longestSubstringIndex = charIndex
              found = false
            }
          }
        }
        charIndex += 1 // goes at the next char
      }
    }
    Right(results)
  }
}