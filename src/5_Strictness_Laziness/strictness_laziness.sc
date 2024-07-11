// Strict function: Always evaluates its arguments
def strictFunction(x: Int): Int = {
  println(s"Running function strict: $x")
  x * x
}

val foo = strictFunction(2 + 3)

//val throwBefore = strictFunction(sys.error("this will evaluate before running strictFunction"))

// Non-strict function: Can choose to evaluate its arguments or not

// explicitly showing lazy arguments are functions
def ifFunction[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = if (cond) onTrue() else onFalse()
ifFunction(true, () => println("evaluated onTrue"), () => println("evaluated onFalse"))

// same thing but with scala syntax
def ifFunction2[A](cond: Boolean, onTrue: => A, onFalse: => A): A = if (cond) onTrue else onFalse
ifFunction2(true, println("evaluated onTrue"), println("evaluated onFalse"))

// lazy keyword
def evaluateTwice(i: => Int): Int = i + i

val x = evaluateTwice({
  println("evaluating i");
  2 + 3
})

def evaluateOnce(i: => Int): Int = {
  // lazy val:
  // delay evaluation until first referenced
  // and cache the result so subsequent references don't trigger revaluation
  lazy val cachedValue = i
  cachedValue + cachedValue
}
val x = evaluateOnce({
  println("evaluating i");
  2 + 3
})

