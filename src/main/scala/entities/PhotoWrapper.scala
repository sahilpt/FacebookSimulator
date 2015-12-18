package entities

/**
 * Created by sahilpt on 12/17/15.
 */
//case class PhotoWrapper (id:Option[Int] = None,
//                         from:Option[Int] = None, /*user identity*/
//                         hiddenValue:String,
//                         album:Option[Int] = None,
//                         can_delete:Option[Boolean] = None
//                          )

case class PhotoWrapper (id:Int,
                         byPage:Int,  /* 1 indicates photo is posted by page otherwise user*/
                         from:Int, /*user identity*/
                         album:Int,
                         can_delete:Boolean,
                         hiddenValue:String,
                         token:String)