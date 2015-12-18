package entities

/**
 * Created by sahilpt on 11/30/15.
 */
case class PhotoUpdate(id:Option[Int] = None,
                       byPage:Option[Int],  /* 1 indicates photo is posted by page otherwise user*/
                       from:Option[Int] = None, /*user identity*/
                       link:Option[String] = None,
                       name:Option[String] = None, /*name of the photo*/
                       album:Option[Int] = None,
                       can_delete:Option[Boolean] = None)
