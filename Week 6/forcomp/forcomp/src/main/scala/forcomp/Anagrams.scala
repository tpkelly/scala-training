package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *  
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *  
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = 
    w.toList.groupBy(x => x.toLower).map(x => (x._1, x._2.length)).toList.sortBy(x => x._1)
 
  /** Converts a sentence into its character occurrence list. */
  //NB: List.mkString("") concats all strings in the list together into one string
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *  
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(wordOccurrences)

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    val option = dictionaryByOccurrences.get(wordOccurrences(word));
    if (option.isEmpty) List()
    else option.get;
  }
  

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   * 
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def doCombinations(occurrences: Occurrences, acc: List[Occurrences]): List[Occurrences] = {
      if (occurrences.isEmpty) acc
      else {
        val extractedItem = List((occurrences.head._1, 1))
        val remainingList = subtract(occurrences, extractedItem)
        val newAccItems = acc.map(item => add(item, occurrences.head._1))
        doCombinations(remainingList, acc ::: newAccItems)
      }
    } 
    
    doCombinations(occurrences, List(List()))
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    if (y.isEmpty) x
    else {
      val matchingItem = x.find(item => item._1 == y.head._1).get
      val matchingIndex = x.indexOf(matchingItem)
      
      if (matchingItem._2 < y.head._2)
        throw new NoSuchElementException() 
      else if (matchingItem._2 == y.head._2)
        subtract(x.take(matchingIndex) ::: x.drop(matchingIndex+1), y.tail)
      else subtract(x.take(matchingIndex) :::
          List((matchingItem._1, matchingItem._2 - y.head._2)) ::: x.drop(matchingIndex+1),
          y.tail)
    }
  }

  def add(x: Occurrences, y: Char) : Occurrences = {
    val matching = x.find(item => item._1 == y)
    if (matching.isEmpty) x :+ (y, 1)
    else {
      val matchingIndex = x.indexOf(matching.get)
      x.take(matchingIndex) ::: List((y, matching.get._2 + 1)) ::: x.drop(matchingIndex + 1)
    }
  }
  
  /** Returns a list of all anagram sentences of the given sentence.
   *  
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *  
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *  
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    
    def getSubwords(sentence: Sentence) : List[Word] = {
      val occurrences = sentenceOccurrences(sentence)
      val combos = combinations(occurrences)
      combos.flatMap(combo => { 
      val option = dictionaryByOccurrences.get(combo);
        if (option.isEmpty) Nil
        else option.get;
      }).distinct
    }
        
    def doAnagram(subwords : List[Word], targetOccurrences: Occurrences, acc : List[Sentence]) : List[Sentence] = {
      if (targetOccurrences.isEmpty) acc
      else subwords.map(word => {
        val wordOcc = wordOccurrences(word)
        try
        {
          val remainingOccurrences = subtract(targetOccurrences, wordOcc)
          doAnagram(subwords, remainingOccurrences, acc.map(item => item :+ word))
        }
        catch { case e : Exception => List() }
      }).flatten
    }
    
    doAnagram(getSubwords(sentence), sentenceOccurrences(sentence), List(List()))
  }
}
