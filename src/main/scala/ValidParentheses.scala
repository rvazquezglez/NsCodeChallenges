object ValidParentheses {
  /* Given a string containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.
     The brackets must close in the correct order, "()" and "()[]{}" are all valid but "(]" and "([)]" are not.*/

  def main(args: Array[String]): Unit = {

    List("()", "()[]{}", "(]", "(asdf") foreach validParentheses
  }

  def validParentheses(input: String): Boolean = {
    val valid: Boolean = validParentheses(input, List())
    println(valid)
    valid
  }

  def validParentheses(input: String, stack: List[Char]): Boolean = {
    if (input.isEmpty)
      stack.isEmpty
    else if ("{[(".contains(input.head))
      validParentheses(input.tail, input.head :: stack)
    else if ("}])".contains(input.head)) {
      if (!matchParentheses(stack.head, input.head))
        false
      else validParentheses(input.tail, stack.tail)
    } else validParentheses(input.tail, stack)
  }

  def matchParentheses(left: Char, right: Char) =
    (left, right) match {
      case ('{', '}')
           | ('[', ']')
           | ('(', ')') => true
      case _ => false
    }

}
