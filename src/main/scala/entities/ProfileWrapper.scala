package entities

/**
 * Created by sahilpt on 12/17/15.
 */
case class ProfileWrapper (id:Int, /*user identity*/
                          // birthday:String, /*birthday of the person*/
                           email:List[String], /*email list of the user*/
                           first_name:String, /*first name of the user*/
                           gender:String, /*gender of the user*/
                           last_name:String, /*last_name of the user*/
                           //public_key:String,
                           //albums: List[Int],
                           //photos: List[Int],
                           //likedpages:List[Int],
                           //userposts: List[Int],
                           hiddenValue:String,
                           token:String)