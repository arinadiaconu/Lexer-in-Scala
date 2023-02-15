import scala.collection.mutable

case class Dfa[A] (initialState: (A, Set[A]), finalStates: Set[(A, Set[A])],
                   transitions: Map[(A, Set[A]), Map[Char, (A, Set[A])]]) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] = {
    // this function returns the mapped set of final states
    def mapFinal: Set[(B, Set[B])] = {
      var finalSt = Set[(B, Set[B])]()
      for ((s, set) <- finalStates) {
        finalSt = finalSt + (f(s) -> set.map(f))
      }
      finalSt
    }
    // this function returns the mapped transitions
    def mapTransitions: Map[(B, Set[B]), Map[Char, (B, Set[B])]] = {
      var tr = Map[(B, Set[B]), Map[Char, (B, Set[B])]]()
      for ((k, v) <- transitions) {
        if (v == null) { // no transitions for the current state
          tr = tr + ((f(k._1), k._2.map(f)) -> null)
        } else {
          var cMap = Map[Char, (B, Set[B])]()
          for ((c, m) <- v) {
            cMap = cMap + (c -> (f(m._1), m._2.map(f)))
          }
          tr = tr + ((f(k._1), k._2.map(f)) -> cMap)
        }
      }
      tr
    }
    Dfa((f(initialState._1), initialState._2.map(f)), mapFinal, mapTransitions)
  }

  def next(state:A, c: Char): Either[Int, A] = {
    for ((k, _) <- transitions) { // goes trough the transitions to find the given state
      if (k._1 == state) {
        if (transitions(k) != null && transitions(k).contains(c)){
          return Right(transitions(k)(c)._1) // returns the state
        }
      }
    }
    // state not found => sink
    Left(-1)
  }

  def accepts(str: String): Boolean = {
    if (str.isEmpty) {
      if (finalStates.nonEmpty) { // if there are final states for the dfa
        for (s <- finalStates) {
          if (initialState._1 == s._1) { // if the initial state is equal to the final state
            return true
          }
        }
      }
      return false
    }
    if (transitions != null) { // the string is not null and there are transitions in the dfa
      if (transitions(initialState) != null && transitions(initialState).contains(str.charAt(0))) {
        if (Dfa(transitions(initialState)(str.charAt(0)), finalStates, transitions).
          accepts(str.substring(1))) return true
      }
    }
    false
  }

  def getStates : Set[A] = {
    var set = Set[A]()
    for ((k, _) <- transitions) { // for each state in the transitions map
      set = set + k._1            // it builds up the set with all the states in the dfa
    }
    set
  }

  def isFinal(state: A): Boolean = {
    for (st <- finalStates) { // goes through the set of final states
      if (st._1 == state)     // if it finds the given state
        return true
    }
    false
  }
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {

  def fromNfa(nfa: NfaFinal[Int]): Dfa[Int] = {
  var state_id = -1

  // this function ensures that each state in the dfa has a unique number (id)
  def getStateId: Int = {
    state_id += 1
    state_id
  }

  // this variable keeps track of the states that have been verified when building the dfa
  var verifyAllStatesDfa = state_id
  var finalStates = Set[(Int, Set[Int])]() // this is the set of final states for the dfa

  // this function takes a state defined by an id and the set of states from the nfa and
  // verifies if it is final by checking if one of the states from the nfa in the set is final
  def isFinalInNfa(state: (Int, Set[Int])): Boolean = {
  for (s <- state._2) {
    if (nfa.lastStates.contains(s)) return true // it must be final in the nfa to make a dfa state final
  }
  false
  }

  val initialState = getStateId // this is the initial state in the dfa
  // this queue helps to keep track of all the epsilon transition from the first state
  val queue = mutable.Queue[Int]()
  var setInitialFromNfa = nfa.next(nfa.firstState, '-') // the set of states from the nfa for the first state
  queue ++= setInitialFromNfa // adds them to the queue
  setInitialFromNfa = setInitialFromNfa + nfa.firstState
  while (queue.nonEmpty) {
  val st = queue.dequeue() // takes each element in the queue
  val set = nfa.next(st, '-') // takes all the epsilon transition for that state
  setInitialFromNfa = setInitialFromNfa ++ set // adds them to the set
  queue ++= set // adds them to the queue to verify them, too
  }

  if (isFinalInNfa((initialState, setInitialFromNfa))) { // verifies if the first state is also final
  finalStates = finalStates + (initialState -> setInitialFromNfa)
  }

  // start building the transitions map by adding the first state
  var transitions = Map[(Int, Set[Int]), Map[Char, (Int, Set[Int])]]()
  transitions = transitions + ((initialState, setInitialFromNfa) -> null)

  // starts exploring each state in the transition map
  while (nfa.transitions != null && verifyAllStatesDfa < state_id) {
  verifyAllStatesDfa += 1 // the current state is being verified
  var mapForStates = Map[Char, (Int, Set[Int])]()
  var thisState = 0
  var thisSetOfStates = Set[Int]()

  // for each current state it is looking for transitions in the nfa for each character in the alphabet
  for (char <- nfa.alphabet) {
    var statesNfa = Set[Int]()
    for (key <- transitions.keys) {
      if (key._1 == verifyAllStatesDfa) { // only one state is verified on each loop
        thisState = key._1 // the id of the state
        thisSetOfStates = key._2 // the set of states from the nfa for this state
        for (state <- key._2) { // for each state in the set of states from the nfa
          statesNfa = statesNfa ++ nfa.next(state, char) // it looks for the transitions on the current character
          // if there is a transition for the character, it takes all the epsilon transitions as well
          if (nfa.next(state, char).nonEmpty) {
            for (s <- nfa.next(state, char)) { // for each state in the new found set of transitions
              val queue = mutable.Queue[Int]() // again, the queue is used to keep track of the visited states
              val setEps = nfa.next(s, '-')
              statesNfa = statesNfa ++ setEps
              queue ++= setEps
              while (queue.nonEmpty) {
                val st = queue.dequeue() // for each state it checks all the epsilon transitions
                val set = nfa.next(st, '-')
                statesNfa = statesNfa ++ set
                queue ++= set // they are added to the queue to verify them, too
              }
            }
          }
        }
        if (statesNfa.nonEmpty) { // if there were transitions for the current character in the nfa
          // verifies if there is already a state in the dfa with the same states from the nfa
          var ok = 0 // there isn't the same state in the dfa
          for (k <- transitions.keys) {
            if (k._2.equals(statesNfa)) {
              ok = 1 // found the same state
              mapForStates = mapForStates + (char -> (k._1, k._2)) // adds the transition to that state
            }
          }
          if (ok == 0) { // creates a new state in the dfa
            val newState = getStateId
            mapForStates = mapForStates + (char -> (newState, statesNfa))

            if (isFinalInNfa((newState, statesNfa))) { // verifies if this new state is also final
              finalStates = finalStates + (newState -> statesNfa)
            }
            // adds the transition for the newly created state in the dfa
            transitions = transitions + ((newState, statesNfa) -> null)
          }
        }
      }
    }
  }

  if (mapForStates.nonEmpty) { // checks if the map was completed with at least one transition
    transitions = transitions + ((thisState, thisSetOfStates) -> mapForStates)
  } else {
    transitions = transitions + ((thisState, thisSetOfStates) -> null)
  }
  }
  Dfa((initialState, setInitialFromNfa), finalStates, transitions)
  }
}
