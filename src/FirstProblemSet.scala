/**
  * Created by romanberla on 04.10.2016.
  */

object FirstProblemSet {
  def main(args:Array[String]): Unit ={
    println(flatten1(List(List(1, 2), List(3,4))))
    println(count(1, List(1, 1, 10, 100, 1)))
    println(replicate("asdf", 10))
    println(sqrList(List(1, 2, 3, 4, 5)))
    println(palindrome(List(1,2, 2,1)))
    println(listLength(List(1, 3, 5, 60, 203, 2, 6)))
  }

  def flatten1[A](inputList:List[List[A]]):List[A] = {
      if (inputList == Nil) Nil
      else inputList.head ++ flatten1(inputList.tail)
  }

  def count[A](inputObject:A, inputList:List[A]):Int =
    if (inputList == Nil) 0
    else if(inputList.head == inputObject) 1 + count(inputObject, inputList.tail)
    else count(inputObject, inputList.tail)

  def replicate[A](inputObject:A, repeatTimes:Int):List[A] =
    if(repeatTimes <= 0) Nil
    else inputObject :: replicate(inputObject, repeatTimes - 1)

  def sqrList(inputList:List[Int]):List[Int] =
    if(inputList == Nil) Nil
    else inputList.head*inputList.head :: sqrList(inputList.tail)

  def palindrome[A](inputList:List[A]):Boolean =
    inputList == inputList.reverse

  def listLength[A](inputList:List[A]):Int =
    if(inputList == Nil) 0
    else 1 + listLength(inputList.tail)



}
