package com.eschloff.captivation

import scala.collection.mutable.ListBuffer

/** A Trie data structure that can store words in lowercase only.
 *  Contains methods to add, check if a word is stored, and search for a word starting with a prefix.
 *  
 *  @constructor create a new empty tree (i.e. one node with no children)
 */
class Trie {
  
  val root: Node = new Node()
  
  /** Constant to subtract the 'a' character's int value from the other lower-case characters to use as an index */
  val correction = 'a'.toInt
  
  /** Adds a word to the Trie, returning true if added successfully and
   *  false if the word was already present.
   *  
   *  @param word the word to add
   *  @return a boolean of whether the word was added (true) or was already present (false)
   */
  def add(word: String): Boolean = {
    var currentNode = root
    var isNewWord = false
    val chars = word.toCharArray()

    chars.foreach(char => {
      val charIndex = char.toInt - correction
      if (currentNode.children(charIndex) == null) {
        currentNode.children(charIndex) = new Node()
        isNewWord = true
      }
      currentNode = currentNode.children(charIndex)
    })
    if (currentNode.isEndOfWord) isNewWord = false else isNewWord = true
    currentNode.isEndOfWord = true
    isNewWord
  }
  
  /** Returns true if the Trie contains the word, or false if not.
   *  
   *  @param word the word to check
   *  @return if the word was in the Trie
   */
  def contains(word: String): Boolean = {
    var currentNode = root
    val chars = word.toCharArray()
    
    chars.foreach(char => {
      val charIndex = char.toInt - correction
      if (currentNode.children(charIndex) == null) return false
      currentNode = currentNode.children(charIndex)
    })
    if (currentNode.isEndOfWord) true else false
  }
  
  /** Returns a list of all the words in the Trie that begin with the specified prefix.
   *  
   *  @param prefix the prefix to use
   *  @return a list of the words stored in the Trie that begin with the prefix.
   */
  def search(prefix: String): List[String] = {
    var currentNode = root
    val chars = prefix.toCharArray()
    val wordList = new ListBuffer[String]()
    
    chars.foreach(char => {
      val charIndex = char.toInt - correction
      if (currentNode.children(charIndex) == null) return wordList.toList
      currentNode = currentNode.children(charIndex)
    })
    
    if (currentNode.isEndOfWord) wordList.append(prefix)
    
    /** currentNode is now pointing to root of sub-trie at last char of prefix */
    
    depthFirstListAssembly(currentNode, wordList, prefix)
    
    wordList.toList
  }
  
  /** Private recursive method to add stored words from the Trie to a list in a depth-first manner.
   *  This is a helper function for Trie.search().
   *  
   *  @param root The top node of the current sub-Trie
   *  @param wordList The list to which words will be added
   *  @param prefix The current prefix to be prepended
   */
  private def depthFirstListAssembly(root: Node, wordList: ListBuffer[String], prefix: String): Unit = {
    var currentNode = root
    for (i <- 0 until currentNode.children.length) {
      val child = currentNode.children(i)
      if (child != null) {
        if (child.isEndOfWord) {
          wordList.append(prefix+(i+correction).toChar)
        }
        depthFirstListAssembly(child, wordList, prefix+(i+correction).toChar)
      }
    }
  }
  
  
  /** Case class for nodes of the Trie.
   *  
   *  @constructor create a new Node using the specified alphabet length
   *  @param alphabetLength the length of the alphabet being used
   */
  case class Node(alphabetLength: Int = 26) {
    var isEndOfWord: Boolean = false
    val children = Array.ofDim[Node](alphabetLength)
  }
  
}
