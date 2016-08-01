package hmrc.scala.exercise.fruits.checkout

/**
  * Created by D Yellapu on 01/08/2016.
  *
  * This class is used to compute the checkout pricing
  * for the apple and orangce fruits store
  */
class FruitsCheckOut(orderFruitArray: Array[String]) {

  //these values are harcoded as per the requirement and timeframe
  val (apple, orange) = ("Apple", "Orange")
  val fruitMap : Map[String, Double] = Map(apple -> 0.60, orange -> 0.25)

  def calculatePrice: Double = {
      calculateFruitPrice(getFruitCount(apple), fruitMap(apple)) +
      calculateFruitPrice(getFruitCount(orange), fruitMap(orange))
  }

  def getFruitCount(offerFruitType: String): Int = {
    var offerFruitsCtr = 0
    for (fruitType <- orderFruitArray) {
      if (fruitType.equalsIgnoreCase(offerFruitType)) offerFruitsCtr += 1
    }
    offerFruitsCtr
  }

  def onePlusOneOffer(fruitType: String): Int = {
    val ctr = getFruitCount(fruitType)
    if (ctr > 1) ctr + ctr / 2 else ctr
  }

  def threeForTwo(fruitType: String): Int = {
    val ctr = getFruitCount(fruitType)
    if (ctr >= 2)
      ctr + ctr / 2
    else
      ctr
  }

  def calculateFruitPrice(fruitCtr : Int, fruitPrice: Double): Double = {
    var price: Double = 0
    for(i <- 0 until fruitCtr)
        price += fruitPrice
    price
  }

  def calculateOfferPrice: Double = {
    calculateFruitPrice(onePlusOneOffer(apple), fruitMap(apple)) +
      calculateFruitPrice(threeForTwo(orange), fruitMap(orange))
  }
}


//This client program used to compute the price of the fruits
//there are 7 tests
// 1 to 5 tests are for step 1
// 6 to 7 tests are for offers test
object FruitsCheckOut {
  def main(args: Array[String]) {
    //test 1
    val fruitsArray = Array("Apple", "Apple", "Orange", "Apple")
    val checkout = new FruitsCheckOut(fruitsArray)
    val totalValue = checkout.calculatePrice
    println(totalValue) //£2.05

    //test 2 //invalid mixed input
    val fruitsArray1 = Array("sadsad", "Apple")
    val checkout1 = new FruitsCheckOut(fruitsArray1)
    val totalValue1 = checkout1.calculatePrice
    println(totalValue1) //calculate apple price only

    //test 3 //only apples
    val fruitsArray2 = Array("Apple", "Apple")
    val checkout2 = new FruitsCheckOut(fruitsArray2)
    val totalValue2 = checkout2.calculatePrice
    println(totalValue2) //£1.2

    //test 4 //only oranges
    val fruitsArray3 = Array("Orange", "Orange")
    val checkout3 = new FruitsCheckOut(fruitsArray3)
    val totalValue3 = checkout3.calculatePrice
    println(totalValue3) //£0.5


    //test 5 //invalid input
    val fruitsArray4 = Array("sadsad", "asdasd")
    val checkout4 = new FruitsCheckOut(fruitsArray4)
    val totalValue4 = checkout4.calculatePrice
    println(totalValue4) //shold return 0.0

    ///////////////////**************Step 2 OFFERS PRICES TESTING *************************///////////////////////////
    //test 6
    val fruitsArray5 = Array("Apple", "Apple", "Orange", "Apple")
    val checkout5 = new FruitsCheckOut(fruitsArray5)
    val totalValue5 = checkout5.calculateOfferPrice
    println(totalValue5) //£2.05

    //test 7
    val fruitsArray6 = Array("Orange", "Orange", "Orange", "Apple")
    val checkout6 = new FruitsCheckOut(fruitsArray6)
    val totalValue6 = checkout6.calculateOfferPrice
    println(totalValue6) //£2.05

  }
}
