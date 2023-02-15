import scala.collection.mutable

case class Nfa[A](firstState: A, lastState: A, alphabet: Set[Char],
                  transitions: Map[A, Map[Char, Set[A]]], token: String, lineInSpec: Int) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] = {
    // this function returns the transition map with the mapped type
    def map_transitions : Map[B, Map[Char, Set[B]]] = {
      var new_transitions = Map[B, Map[Char, Set[B]]]()
      for ((s, t) <- transitions) { // for each pair of (state, Map(char, set of states))
        var new_map = Map[Char, Set[B]]()
        for ((c, m) <- t) { // for each pair of (char, set of states)
          var set = Set[B]()
          for (e <- m) { // for each element in the set
            set = set + f(e) // builds up the new set
          }
          new_map  = new_map + (c -> set)
        }
        new_transitions = new_transitions + (f(s) -> new_map) // builds up the new map
      }
      new_transitions
    }
    Nfa(f(firstState), f(lastState), alphabet, map_transitions, token, lineInSpec)
  }

  def next(state:A, c: Char): Set[A] = {
    var value = Map[Char, Set[A]]()
    // checks if the nfa has transitions and if they contain the given state
    if ((transitions != null) && transitions.contains(state)) {
      value = transitions(state) // keeps the map from the given state
    } else return Set() // otherwise returns an empty set
    if (value.contains(c)) { // if there are transitions for the given char
      return value(c)
    }
    Set()
  }

  def accepts(str: String): Boolean = {
    if (str.isEmpty) {
      if (firstState == lastState) return true
      else if ((transitions != null) && transitions.contains(firstState)) {
        // if the string is empty, it checks for all the epsilon transitions toward the final state
        if (transitions(firstState).contains('-')) {
          for (s <- transitions(firstState)('-')) {
            if (Nfa(s, lastState, alphabet, transitions, token, lineInSpec).accepts(str)) return true
          }
          return false // the epsilon transitions didn't end with the final state
        } else return false // no epsilon transitions
      } else return false // no more transitions to explore
    }
    // the string isn't empty
    if (transitions.contains(firstState)) {
      if (transitions(firstState).contains(str.charAt(0))) { // checks for transitions for the first character
        for (s <- transitions(firstState)(str.charAt(0))) {
          if (Nfa(s, lastState, alphabet, transitions, token, lineInSpec).accepts(str.substring(1))) return true
        }
        false
      } else if (transitions(firstState).contains('-')) { // checks for transitions for epsilon
        for (s <- transitions(firstState)('-')) {
          if (Nfa(s, lastState, alphabet, transitions, token, lineInSpec).accepts(str)) return true
        }
        false
      } else false
    } else false
  }

  def getStates : Set[A] = {
    var set = Set(firstState, lastState)
    for ((k,v) <- transitions) { // goes through the transitions map to get all the states
      set = set + k
      for((_,s) <- v) { // goes through all the sets in the map
        set = set ++ s
      }
    }
    set
  }

  def isFinal(state: A): Boolean = {
    state == lastState
  }
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {
  def fromPrenex(str: String, token: String, lineInSpec: Int, index: Int): Nfa[Int] = {
    var strings = Array[String]() // this will keep the strings from the given split prenex
    if (str.contains("'") && str.length > 1) {
      if(str.charAt(0) == '\'' && str.charAt(2) == '\'') {
        strings = strings :+ str.charAt(1).toString
      }
      else {
        strings = str.split(" ")
      }
    } else {
      strings = str.split(" ")
    }

    val stringStack = mutable.Stack[String]() // each string will be put in a string stack
    var ok = 0
    for (string <- strings) {
      if (string == "'" && ok == 0) ok = 1
      else if (string == "'" && ok == 1) {
        stringStack.push(" ") // pushing whitespace on the stack
        ok = 0
      }
      else stringStack.push(string)
    }

    val NfaStack = mutable.Stack[Nfa[Int]]() // nfa stack
    var alphabet: Set[Char] = Set[Char]()    // the alphabet for the nfa

    var state_id = index
    // this function ensures that each state in the nfa has a unique number (id)
    def getStateId: Int = {
      state_id += 1
      state_id
    }

    while (stringStack.nonEmpty) {
      // for each string the string stack it verifies if it matches one of
      // the possible key words in a prenex or if it is a simple character
      val string = stringStack.pop()
      string match {
        case "CONCAT" =>
          // CONCAT needs 2 arguments and takes the first 2 elements in the nfa stack
          val x = NfaStack.pop()
          val y = NfaStack.pop()

          // starts building the nfa with the transitions
          var transition = Map[Int, Map[Char, Set[Int]]]()
          if (x.transitions == null) {   // the first nfa doesn't have transitions
            if (y.transitions != null) { // only the second nfa has transitions
              transition = y.transitions // the new nfa will only have the second nfa's transitions
            }
          } else {
            if (y.transitions == null) {
              transition = x.transitions // the new nfa will only have the first nfa's transitions
            } else {
              transition = x.transitions ++ y.transitions // the new nfa will have the transitions from both nfas
            }
          }
          // CONCAT puts only one epsilon transition between the two nfas
          val setOfSates = Set(y.firstState)
          var statesForChar = Map[Char, Set[Int]]()
          statesForChar = statesForChar + ('-' -> setOfSates)
          transition = transition + (x.lastState -> statesForChar)

          NfaStack.push(Nfa(x.firstState, y.lastState, alphabet, transition, token, lineInSpec))
        case "UNION" =>
          // UNION needs 2 arguments and takes the first 2 elements in the nfa stack
          val x = NfaStack.pop()
          val y = NfaStack.pop()
          // UNION builds 2 more states - one that becomes the initial state and one that becomes the final state
          val newFirstState = getStateId
          val setOfSatesForFirst = Set(x.firstState, y.firstState)
          var statesForFirst = Map[Char, Set[Int]]()
          statesForFirst = statesForFirst + ('-' -> setOfSatesForFirst)

          val newLastState = getStateId
          val setOfFStates = Set(newLastState)
          var statesForFChar = Map[Char, Set[Int]]()
          statesForFChar = statesForFChar + ('-' -> setOfFStates)

          val setOfSStates = Set(newLastState)
          var statesForSChar = Map[Char, Set[Int]]()
          statesForSChar = statesForSChar + ('-' -> setOfSStates)

          // starts building the nfa with the transitions
          var transitions = Map[Int, Map[Char, Set[Int]]]()
          if (x.transitions == null) {
            if(y.transitions != null) {
              transitions = y.transitions // the new nfa will only have the second nfa's transitions
            }
          } else {
            if (y.transitions == null) {
              transitions = x.transitions // the new nfa will only have the first nfa's transitions
            } else {
              transitions = x.transitions ++ y.transitions // the new nfa will have the transitions from both nfas
            }
          }

          transitions = transitions + (newFirstState -> statesForFirst)
          transitions = transitions + (x.lastState -> statesForFChar)
          transitions = transitions + (y.lastState -> statesForSChar)

          NfaStack.push(Nfa(newFirstState, newLastState, alphabet, transitions, token, lineInSpec))
        case "STAR" =>
          // STAR needs 1 argument and takes the first element in the nfa stack
          val x = NfaStack.pop()
          // STAR builds 2 more states - one that becomes the initial state and one that becomes the final state
          val newFirstState = getStateId
          val newLastState = getStateId

          val setOfSatesForFirst = Set(x.firstState, newLastState)
          var statesForFirst = Map[Char, Set[Int]]()
          statesForFirst = statesForFirst + ('-' -> setOfSatesForFirst)

          val setOfStatesForLast = Set(newLastState, x.firstState)
          var statesForLast = Map[Char, Set[Int]]()
          statesForLast = statesForLast + ('-' -> setOfStatesForLast)

          // starts building the nfa with the transitions
          var transitions = Map[Int, Map[Char, Set[Int]]]()
          if (x.transitions != null) {  // checks if the old nfa has any transitions
            transitions = x.transitions // the new nfa will have the old nfa's transitions
          }

          transitions = transitions + (newFirstState -> statesForFirst)
          transitions = transitions + (x.lastState -> statesForLast)

          NfaStack.push(Nfa(newFirstState, newLastState, alphabet, transitions, token, lineInSpec))
        case "PLUS" =>
          // PLUS needs 1 argument and takes the first element in the nfa stack
          val x = NfaStack.pop()
          // PLUS builds 2 more states - one that becomes the initial state and one that becomes the final state
          val newFirstState = getStateId
          val newLastState = getStateId

          val setOfSatesForFirst = Set(x.firstState)
          var statesForFirst = Map[Char, Set[Int]]()
          statesForFirst = statesForFirst + ('-' -> setOfSatesForFirst)

          val setOfStatesForLast = Set(newLastState, x.firstState)
          var statesForLast = Map[Char, Set[Int]]()
          statesForLast = statesForLast + ('-' -> setOfStatesForLast)

          // starts building the nfa with the transitions
          var transitions = Map[Int, Map[Char, Set[Int]]]()
          if (x.transitions != null) {  // checks if the old nfa has any transitions
            transitions = x.transitions // the new nfa will have the old nfa's transitions
          }

          transitions = transitions + (newFirstState -> statesForFirst)
          transitions = transitions + (x.lastState -> statesForLast)

          NfaStack.push(Nfa(newFirstState, newLastState, alphabet, transitions, token, lineInSpec))
        case "MAYBE" =>
          // MAYBE needs 1 argument and takes the first element in the nfa stack
          val x = NfaStack.pop()
          // MAYBE builds 2 more states - one that becomes the initial state and one that becomes the final state
          val newFirstState = getStateId
          val newLastState = getStateId

          val setOfSatesForFirst = Set(x.firstState, newLastState)
          var statesForFirst = Map[Char, Set[Int]]()
          statesForFirst = statesForFirst + ('-' -> setOfSatesForFirst)

          val setOfStatesForLast = Set(newLastState)
          var statesForLast = Map[Char, Set[Int]]()
          statesForLast = statesForLast + ('-' -> setOfStatesForLast)

          // starts building the nfa with the transitions
          var transitions = Map[Int, Map[Char, Set[Int]]]()
          if (x.transitions != null) {  // checks if the old nfa has any transitions
            transitions = x.transitions // the new nfa will have the old nfa's transitions
          }

          transitions = transitions + (newFirstState -> statesForFirst)
          transitions = transitions + (x.lastState -> statesForLast)

          NfaStack.push(Nfa(newFirstState, newLastState, alphabet, transitions, token, lineInSpec))
        case "eps" =>
          // builds the nfa for epsilon which has only one state that is both initial and final and has no transitions
          val thisFirstState = getStateId
          NfaStack.push(Nfa(thisFirstState, thisFirstState, alphabet, null, token, lineInSpec))
        case "void" =>
          // builds the nfa for the void language which has an initial state and a final state and no transitions
          val thisFirstState = getStateId
          val thisLastState = getStateId
          NfaStack.push(Nfa(thisFirstState, thisLastState, alphabet, null, token, lineInSpec))
        case _ => // in the end it matches any char
          if (string.length == 1) { // verifies if it is indeed a char
            alphabet = alphabet + string.charAt(0)
            // builds the nfa for the char which has an initial state
            // and a final state and a transition between them for that char
            val thisFirstState = getStateId
            val thisLastState = getStateId

            var transition = Map[Int, Map[Char, Set[Int]]]()
            var statesForChar = Map[Char, Set[Int]]()
            val setOfSates = Set(thisLastState)
            statesForChar = statesForChar + (string.charAt(0) -> setOfSates)

            transition = transition + (thisFirstState -> statesForChar)

            NfaStack.push(Nfa(thisFirstState, thisLastState, alphabet, transition, token, lineInSpec))
          }
      }
    }

    NfaStack.pop()
  }
}
