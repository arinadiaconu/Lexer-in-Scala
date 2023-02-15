case class NfaFinal[A](firstState: A, lastStates: Set[A], alphabet: Set[Char],
                  transitions: Map[A, Map[Char, Set[A]]]) {
  // this is the final nfa from all the nfas derived from the regexes given
  // what is special about it is that it has multiple final states

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
      for (s <- lastStates) {
        if (firstState == s) return true
        else if ((transitions != null) && transitions.contains(firstState)) {
          // if the string is empty, it checks for all the epsilon transitions toward the final states
          if (transitions(firstState).contains('-')) {
            for (ss <- transitions(firstState)('-')) {
              if (NfaFinal(ss, lastStates, alphabet, transitions).accepts(str)) return true
            }
            return false // the epsilon transitions didn't end with the final state
          } else return false // no epsilon transitions
        } else return false // no more transitions to explore
      }
    }
    // the string isn't empty
    if (transitions.contains(firstState)) {
      if (transitions(firstState).contains(str.charAt(0))) { // checks for transitions for the first character
        for (s <- transitions(firstState)(str.charAt(0))) {
          if (NfaFinal(s, lastStates, alphabet, transitions).accepts(str.substring(1))) return true
        }
        false
      } else if (transitions(firstState).contains('-')) { // checks for transitions for epsilon
        for (s <- transitions(firstState)('-')) {
          if (NfaFinal(s, lastStates, alphabet, transitions).accepts(str)) return true
        }
        false
      } else false
    } else false
  }

  def getStates : Set[A] = {
    var set = lastStates + firstState
    for ((k,v) <- transitions) { // goes through the transitions map to get all the states
      set = set + k
      for((_,s) <- v) { // goes through all the sets in the map
        set = set ++ s
      }
    }
    set
  }

  def isFinal(state: A): Boolean = {
    lastStates.contains(state)
  }
}
