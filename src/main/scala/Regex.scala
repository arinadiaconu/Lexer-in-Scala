import scala.collection.mutable

object Regex {
  /*
    this function:
    -> classifies input as either character (Right()) or operator (Left())
    -> converts special inputs like [0-9] to their correct form
    -> converts escaped characters
  */
  def preprocess(s:List[Char]): List[Either[Char,Char]] = {
    var final_list = List[Either[Char,Char]]() // the returned list
    var i = 0

    def check_concat(): Unit = { // this function checks if the concat operator must be put before any character
      if (final_list.nonEmpty && (final_list.head == Left(')') || final_list.head == Left(']')
        || final_list.head == Left('*'))) {
        final_list = Left('.') :: final_list // adds the concat operator ('.') to the list
      }
    }

    while (i < s.length) { // goes through the given list of chars
      if (s(i) == '*') final_list = Left('*') :: final_list // the star operator
      else if (s(i) == '|') final_list = Left('|') :: final_list // the union operator

      else if (s(i) == '\'' && s(i+2) == '\'') { // an escaped character
        check_concat()
        final_list = Right(s(i+1)) :: final_list // adds the escaped character
        // checks whether the concat operator must follow the character
        if (((i + 3) < s.length) && (s(i + 3) != ')') && (s(i + 3) != '*') && (s(i + 3) != '|')) {
          final_list = Left('.') :: final_list
        }
        i = i + 2 // skips the escaping characters ('\'')

      } else if (s(i) == '[') { // special inputs like [0-9]
        var k = i // this variable will keep the position of the right parenthesis (']')
        while (s(k) != ']') {
          k = k + 1
        }
        var index_start = s(i + 1) // the first value of the special input
        val index_finish = s(k - 1) // the last value of the special input
        // the case where the special input is followed by '+' or '?'
        if ((k + 1) < s.length && (s(k+1) == '?' || s(k+1) == '+')) {
          final_list = Left('(') :: final_list
        }
        // separates the new correct form of the input from the rest of the list of chars
        final_list = Left('(') :: final_list
        while (index_start < index_finish) { // builds up the union from the special input
          final_list = Right(index_start) :: final_list
          final_list = Left('|') :: final_list
          index_start = (index_start.toInt + 1).toChar
        }
        final_list = Right(index_start) :: final_list
        final_list = Left(')') :: final_list
        if ((k + 1) < s.length && s(k+1) == '+') { // the plus operator after the input builds the regex (e.e*)
          var index_start = s(i + 1)
          val index_finish = s(k - 1)
          final_list = Left('.') :: final_list
          final_list = Left('(') :: final_list
          while (index_start < index_finish) {
            final_list = Right(index_start) :: final_list
            final_list = Left('|') :: final_list
            index_start = (index_start.toInt + 1).toChar
          }
          final_list = Right(index_start) :: final_list
          final_list = Left(')') :: final_list
          final_list = Left('*') :: final_list
          final_list = Left(')') :: final_list
          i = k + 1 // skips the entire special input and the operator '+'
        } else if ((k + 1) < s.length && s(k+1) == '?') { // the maybe operator builds the regex (e|eps)
          final_list = Left('|') :: final_list
          final_list = Right('~') :: final_list // '~' is eps
          final_list = Left(')') :: final_list
          i = k + 1 // skips the entire special input and the operator '?'
        } else i = k // skips the entire special input

      } else if (s(i) == ')') { // expressions in parenthesis
        // if the expression is followed by '+' or '?'
        if ((i+1) < s.length && (s(i+1) == '+' || s(i+1) == '?')) {
          final_list = Left(')') :: final_list
          var j = i - 1 // this variable will keep the position of the left parenthesis ('(')
          while (s(j) != '(') {
            j = j - 1
          }
          // separates the new expression from the rest of the list of chars
          final_list = final_list.take(j) ++ (Left('(') :: final_list.drop(j))
          val prefix_list = final_list.take(i - j - 1) // the expression in parenthesis
          if (s(i+1) == '+') { // the plus operator builds (e.e*)
            final_list = Left('.') :: final_list
            final_list = prefix_list ::: final_list
            final_list = Left('*') :: final_list
          } else if (s(i+1) == '?') { // the maybe operator builds (e|eps)
            final_list = Left('|') :: final_list
            final_list = Right('~') :: final_list // eps
          }
          final_list = Left(')') :: final_list
          i = i + 1 // skips the '+' or '?'
        } else { // no special operator after the expression in parenthesis
          final_list = Left(')') :: final_list // just adds it to the list
        }

      } else { // any character
        if (s(i) == '(') { // the opening parenthesis
          check_concat()
          final_list = Left(s(i)) :: final_list // adds it to the list
        } else if (s(i) == 'e' && (i+2) <= s.length && s(i+1) == 'p' && s(i+2) == 's') { // epsilon
          check_concat()
          final_list = Right('~') :: final_list // eps
          i = i + 2 // skips the 'p' and 's'
        } else if (((i+1) < s.length) && (s(i+1) == '+' || s(i+1) == '?')) {
          // if the character is followed by the plus or maybe operator
          check_concat()
          final_list = Left('(') :: final_list
          final_list = Right(s(i)) :: final_list
          if (s(i+1) == '+') { // the plus operator builds (c.c*)
            final_list = Left('.') :: final_list
            final_list = Right(s(i)) :: final_list
            final_list = Left('*') :: final_list
          } else if (s(i+1) == '?') { // the maybe operator builds (c|eps)
            final_list = Left('|') :: final_list
            final_list = Right('~') :: final_list // eps
          }
          final_list = Left(')') :: final_list
          i = i + 1 // skips '+' or '?'
        } else if (((i+1) < s.length) && (s(i+1) != ')') && (s(i+1) != '*') && (s(i+1) != '|')) {
          check_concat()
          final_list = Right(s(i)) :: final_list
          final_list = Left('.') :: final_list
        } else {
          check_concat()
          final_list = Right(s(i)) :: final_list
        }
      }
      i = i + 1
    }

    final_list // this final list has all processed characters in the reversed order
  }

  // this function constructs a prenex expression out of a normal one
  def toPrenex(str: String): String = {
    var fin = ""
    var ok = true
    if (str.charAt(str.length - 1).equals('+') && str.charAt(0).equals('(') &&
      str.charAt(str.length - 2).equals(')')) {
      val subStr = str.substring(1, str.length - 2)
      fin = subStr + '(' + subStr + ')' + '*'
      ok = false
    }
    var normal_expression = List[Either[Char, Char]]()
    if (ok)    normal_expression = (Left(')') :: preprocess(str.toList)) ++ List(Left('('))
    else normal_expression = (Left(')') :: preprocess(fin.toList)) ++ List(Left('('))
    var prenex = "" // this will be the postfix regex
    val character_stack = mutable.Stack[Char]()

    def priority(ch: Char): Int = { // this function returns the priority of each operator
      if (ch == '*') return 3
      else if (ch == '.') return 2
      else if (ch == '|') return 1
      0
    }

    var i = 0
    while (i < normal_expression.length) { // goes through the preprocessed list of chars
      if (normal_expression(i).isRight) { // an operand
        prenex = prenex + normal_expression(i).getOrElse("#")
      } else if (normal_expression(i) == Left(')')) { // this marks the beginning of an expression in parenthesis
        character_stack.push(')') // pushes it onto the stack
      } else if (normal_expression(i) == Left('(')) { // this marks the end of an expression in parenthesis
        while (character_stack.top != ')') { // pops all chars until the beginning of that expression is found
          prenex = prenex + character_stack.top
          character_stack.pop()
        }
        character_stack.pop() // pops ')' (the beginning of the expression in parenthesis)
      } else if (normal_expression(i).isLeft) { // an operator
        if (normal_expression(i) == Left('*')) { // this operator has the highest priority
          // the while condition must be '<=' because the highest priority cannot be less than any other priority
          while (priority(normal_expression(i).left.getOrElse('#')) <= priority(character_stack.top)) {
            prenex = prenex + character_stack.top
            character_stack.pop()
          }
        } else { // for all others operators
          while (priority(normal_expression(i).left.getOrElse('#')) < priority(character_stack.top)) {
            prenex = prenex + character_stack.top
            character_stack.pop()
          }
        }
        character_stack.push(normal_expression(i).left.getOrElse('#')) // pushes the current operator onto the stack
      }
      i = i + 1
    }

    while (character_stack.nonEmpty) { // empties the stack
      prenex = prenex + character_stack.top
      character_stack.pop()
    }

    var final_prenex = "" // this is the string that represents the returned prenex
    for (c <- prenex.reverse) { // prenex is in postfix and it reverses it in order to build the wanted prefix form
      if (c == '*') final_prenex = final_prenex + "STAR "
      else if (c == '.') final_prenex = final_prenex + "CONCAT "
      else if (c == '|') final_prenex = final_prenex + "UNION "
      else if (c == '~') final_prenex = final_prenex + "eps "
      else if (c == ' ') final_prenex = final_prenex + "\' \' "
      else final_prenex = final_prenex + c + ' '
    }
    final_prenex.dropRight(1) // removes the last whitespace
  }
}
