package entities

/**
 * Created by gokul on 11/29/15.
 */
case class Profile(id:Int, /*user identity*/
                   birthday:String, /*birthday of the person*/
                   email:List[String], /*email list of the user*/
                   first_name:String, /*first name of the user*/
                   gender:String, /*gender of the user*/
                   last_name:String, /*last_name of the user*/
                   public_key:String,
                   albums: List[Int],
                   photos: List[Int],
                   likedpages:List[Int],
                   userposts: List[Int]) /*public_key of the user*/
