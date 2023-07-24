package T6

object Q1 {
  def Encryption(plainText:String, shift:Int): String = {
    var str: String = "";
    for(c<-plainText) {
      if((c.toUpper)>=65 && (c.toUpper)<=90){ // A-65 Z-90 a-97 z-122
        val base=if(c.isUpper) 'A' else 'a'
        val ch=((c - base + shift) % 26 + base).toChar;
        str=str.concat(ch+"");
      }else{
        str=str.concat(c+"");
      }
    }
    str;
  }

  def Decryption(plainText:String, shift:Int): String={
    var str:String="";
    for (c <- plainText) {
      if ((c.toUpper) >= 65 && (c.toUpper) <= 90) { // A-65 Z-90 a-97 z-122
        val base = if (c.isUpper) 'A' else 'a'
        val ch =((c - base - shift + 26) % 26 + base).toChar;
        str = str.concat(ch + "");
      } else {
        str = str.concat(c + "");
      }
    }
    str;
  }

  def Cipher(plainText:String, shift:Int, operation:(String,Int)=>String): String = {
    operation(plainText,shift);
  }

  def main(args:Array[String]): Unit = {
    print("Insert The Text :");
    val plainText=scala.io.StdIn.readLine();
    val str1=Cipher(plainText,5,Encryption);
    val str2=Cipher(plainText,6,Decryption);

    println("Encryption :"+str1);
    println("Decryption :"+str2);


  }
}
