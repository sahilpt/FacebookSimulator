import java.security.PublicKey

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import entities._
import org.apache.commons.codec.binary.Base64
import org.json4s.{DefaultFormats, Formats}
import spray.client.pipelining._
import spray.http._
import spray.httpx.Json4sSupport

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success};

/*
import spray.json.{JsonFormat, DefaultJsonProtocol}

object FacebookJsonProtocol extends DefaultJsonProtocol {
  implicit val profileFormat = jsonFormat2(Profile)
  implicit val postFormat = jsonFormat2(entities.Post)
  //implicit def googleApiResultFormat[T :JsonFormat] = jsonFormat2(GoogleApiResult.apply[T])
}
*/

/**
 * Created by gokul on 11/30/15.
 */

class NewClient (host: String , bindport: Int, id:Int, aggressionLevel: Int, isClient:Boolean, isPage:Boolean, numClients:Int,parent:ActorRef/*3: Reader 2: Normal User 1: Aggressive poster*/) extends Actor with Json4sSupport {
  implicit val system = context.system

  import system.dispatcher

  var userID = 0

  /**
   * Security Related Information.
   */
  var symmetricKey = GenerateRandomStuff.getString(32)
  var initVector = "RandomInitVector"
  var key = RSAEncryptor.generateKey();

  var myProfile: Profile = new Profile(0, "1988-02-09",List("inClient@dummy.com"), "dummy", "Male", "dummy", "publick_key", List(), List(),List(),List())
  var friendList: FriendList = new FriendList(0, List())
  var newsFeed = new ArrayBuffer[UserPost]()
  var myPage1: Page = new Page(0,"",0,0,List(),"",0,List(),List(),List())
  var failedAttempts: Int = 0
  implicit val timeout = Timeout(100 seconds)


  var myPhoto:Photo = new Photo(0,0,0,"","",0,false)
  var myPost:UserPost = new UserPost(0,0,0,"","","","",List())
  var myAlbum:Album = new Album(0,0,0,"",false,"",List())
  var myFriendList:FriendList = new FriendList(0, List())
  var myPage:Page = new Page(0,"",0,0,List(),"",0,List(),List(),List())

  var myHiddenPhoto:PhotoWrapper = new PhotoWrapper(0,0,0,0,false,"","")
  var myHiddenPost:UserPostWrapper = new UserPostWrapper(0,0,0,"","","")
  var myHiddenProfile:ProfileWrapper = new ProfileWrapper(0,List(),"","","","","")
  var myHiddenPage:PageWrapper = new PageWrapper(0,0,0,"",0,"","")
  var tokenToSend:Authentication = new Authentication(userID, "")
  val pipeline: HttpRequest => Future[HttpResponse] = sendReceive

  val stringPipeline: HttpRequest => Future[Option[String]] = (
    addHeader("X-My-Special-Header", "fancy-value")
      ~> sendReceive
      ~> unmarshal[Option[String]]
    )
  val tokenPipeline: HttpRequest => Future[Authentication] = (
    addHeader("X-My-Special-Header", "fancy-value")
      ~> sendReceive
      ~> unmarshal[Authentication]
    )

  val profilePipeline: HttpRequest => Future[ProfileWrapper] = (
    addHeader("X-My-Special-Header", "fancy-value")
      ~> sendReceive
      ~> unmarshal[ProfileWrapper]
    )
  val friendPipeline: HttpRequest => Future[FriendList] = (
    addHeader("X-My-Special-Header", "fancy-value")
      ~> sendReceive
      ~> unmarshal[FriendList]
    )
  val postPipeline: HttpRequest => Future[UserPostWrapper] = (
    addHeader("X-My-Special-Header", "fancy-value")
      ~> sendReceive
      ~> unmarshal[UserPostWrapper]
    )
  val pagePipeline: HttpRequest => Future[PageWrapper] = (
    addHeader("X-My-Special-Header", "fancy-value")
      ~> sendReceive
      ~> unmarshal[PageWrapper]
    )
  val photoPipeline: HttpRequest => Future[PhotoWrapper] = (
    addHeader("X-My-Special-Header", "fancy-value")
      ~> sendReceive
      ~> unmarshal[PhotoWrapper]
    )
  val albumPipeline: HttpRequest => Future[Album] = (
    addHeader("X-My-Special-Header", "fancy-value")
      ~> sendReceive
      ~> unmarshal[Album]
    )
  var name = self.actorRef.path.name
  var interval = Math.pow(10, aggressionLevel).toInt
  var serverPublicKey:PublicKey = key.getPublic
  var tokenNum:Long = 0

  def getToken():Unit = {
    val response:Future[Authentication] = tokenPipeline(Get("http://localhost:5001/getSecureToken/"+userID))
    response.onComplete{
      case Success(rsp)=>
        var token:String = rsp.token
        println("token is "+token)
        var num = RSAEncryptor.decrypt(Base64.decodeBase64(token), key.getPrivate)
        println("Decrypted Number is " +num)
        tokenNum = num.toString.toLong
        var tokenString = RSAEncryptor.signContent(tokenNum, key.getPrivate)
        tokenToSend = new Authentication(userID, tokenString)
        self ! "DoSomethingNew"
      case Failure(error) =>
        println("Error "+error)
        self ! "GetToken"
    }
  }
  override def receive: Receive = {
    case "GetToken" =>
      getToken()

    case "CreateProfile" =>
      userID = Random.nextInt(1000000)
      var profile: Profile = new Profile(userID, GenerateRandomStuff.getDOB, List(GenerateRandomStuff.getEmail),
        GenerateRandomStuff.getName, GenerateRandomStuff.getGender(Random.nextInt(2)), GenerateRandomStuff.getName, List(), List(), List(), List(), key.getPublic.toString, GenerateRandomStuff.getName)
      self ! AddProfile(profile)
      myProfile = profile
      context.system.scheduler.scheduleOnce(interval milliseconds, self, StartSimulatingUserBehavior)

    case "CreatePage" =>
      var publickey = RSAEncryptor.getPublicKeyString(key.getPublic)
      userID = Random.nextInt(1000000)
      var page: Page = new  Page(userID, GenerateRandomStuff.getName, 0, 0, List(GenerateRandomStuff.getEmail),
        GenerateRandomStuff.getName,GenerateRandomStuff.randBetween(100,10000),, List(), List(), List(), GenerateRandomStuff.getName)
      self ! AddPage(page)
      myPage = page
      context.system.scheduler.scheduleOnce(interval milliseconds, self, "StartSimulatingPageBehavior")

    case "DoSomethingNew" =>
      val noOfChoices = 6
      var choice = Random.nextInt(noOfChoices)
      choice match {
        case 0 =>
          self ! UpdateProfile(myProfile)
        case 1 =>
          self ! GetPost
        case 2 =>
          if (myFriendList.list.nonEmpty) {
            var pickFrnd = myFriendList.list(Random.nextInt(myFriendList.list.length))
            self ! GetFriendProfile(pickFrnd)
          }
        case 3 =>
          if (myProfile.albums.nonEmpty){
            var pickAlbum = myProfile.albums(Random.nextInt(myProfile.albums.length))
            self ! GetAlbum(pickAlbum)
          }
        case 4 =>
          if (myProfile.photos.nonEmpty){
            var pickPhoto = myProfile.photos(Random.nextInt(myProfile.photos.length))
            self ! GetPhoto(pickPhoto)
          }
        case 5 =>
          var dummyAlbumID = Random.nextInt(numClients)
          self ! GetAlbum(dummyAlbumID)
      }
      context.system.scheduler.scheduleOnce(interval milliseconds, self, StartSimulatingUserBehavior)

    case AddFriend(userId: FriendListUpdate) =>
      if(name.toInt != userId) //cannot add self to friend list
      {
        val response: Future[FriendList] = friendPipeline(Put("http://localhost:5001/friendLists/" + name, userId))
        response.onComplete {
          case Success(response) =>
            //println("Returned Success " + response)
            myFriendList = response
            failedAttempts = 0
          case Failure(error) =>
            ////println(error, "failed to get profile")
            if (failedAttempts == 5)
              context.stop(self)
            else failedAttempts += 1
            context.system.scheduler.scheduleOnce(999 milliseconds, self, GetFriends)
        }
      }
    case UpdateProfile(profile:Profile) =>
      val response: Future[Profile] = profilePipeline(Put("http://localhost:5001/profiles/"+name, profile))
      response.onComplete{
        case Success(profile) =>
          ////println("Returned Success "+profile)
          myProfile = profile //Update current profile with updated profile
          failedAttempts = 0
        case Failure(error) =>
        //Update failed
        ////println(error, "failed to get profile")
      }


    case GetFriends =>
      val response: Future[FriendList] = friendPipeline(Get("http://localhost:5001/friendLists/"+name))
      response.onComplete{
        case Success(response) =>
          //println("Returned Success "+response)
          myFriendList = response
          failedAttempts = 0
        //self ! AddFriend(FriendListUpdate(list = 2))
        case Failure(error) =>
          ////println(error, "failed to get profile")
          if (failedAttempts == 5) {
            failedAttempts += 1
            context.system.scheduler.scheduleOnce(999 milliseconds, self, GetFriends)
          }
      }
      sender ! parent

    case GetProfile =>
      ////println("Name of actor"+name)
      val response: Future[ProfileWrapper] = profilePipeline(Get("http://localhost:5001/profiles/"+name))
      response.onComplete{
        case Success(profile) =>
          //println("Returned Success "+profile)
          val token:String = "Token"
          myProfile = AESEncryptor.decrypt(symmetricKey, initVector, profile.hiddenValue).asInstanceOf[Profile]
          //self ! GetFriends
          failedAttempts = 0
        case Failure(error) =>
          ////println(error, "failed to get profile")
          if (failedAttempts == 5) {
            failedAttempts += 1
            context.system.scheduler.scheduleOnce(999 milliseconds, self, GetProfile)
          }
      }
      sender ! parent
    //case UpdateProfile =>
    case GetFriendProfile(userID:Int) =>
      val response: Future[ProfileWrapper] = profilePipeline(Get("http://localhost:5001/profiles/"+userID))
      response.onComplete{
        case Success(profile) =>
          //println("Returned Success "+profile)
          //myProfile = profile
          val token:String = "Token"
          myProfile = AESEncryptor.decrypt(symmetricKey, initVector, profile.hiddenValue).asInstanceOf[Profile]
          //self ! GetFriends
          failedAttempts = 0
        case Failure(error) =>
          ////println(error, "failed to get profile")
      }


    case GetAlbum(albumID) =>
      ////println("Name of actor"+name)
      val response: Future[Album] = albumPipeline(Get("http://localhost:5001/albums/"+albumID))
      response.onComplete{
        case Success(album) =>
          ////println("Returned Success "+album)
          myAlbum = album
          //self ! GetFriends
          failedAttempts = 0
        case Failure(error) =>
          ////println(error, "failed to get album")
      }

    case GetPage =>
      ////println("Name of actor"+name)
      val response: Future[PageWrapper] = pagePipeline(Get("http://localhost:5001/pages/"+name))
      response.onComplete{
        case Success(page) =>
          //println("Returned Success "+page)
          val token:String = "Token"
          myPage = AESEncryptor.decrypt(symmetricKey, initVector, page.hiddenValue).asInstanceOf[Page]
          //self ! GetFriends
          failedAttempts = 0
        case Failure(error) =>
          ////println(error, "failed to get page")
          if (failedAttempts == 5) {
            context.stop(self)
            //println("Shutting down actor "+name)
          }
          else failedAttempts += 1
          context.system.scheduler.scheduleOnce(999 milliseconds, self, GetPage)
      }

    case GetPhoto(photoID) =>
      ////println("Name of actor"+name)
      val response: Future[PhotoWrapper] = photoPipeline(Get("http://localhost:5001/photos/"+photoID))
      response.onComplete{
        case Success(photo) =>
          ////println("Returned Success "+photo)
          //myHiddenPhoto = photo
          val token:String = "Token"
          myPhoto = AESEncryptor.decrypt(symmetricKey, initVector, photo.hiddenValue).asInstanceOf[Photo]
          //val tempPhoto: PhotoWrapper = new PhotoWrapper(photo.id, photo.byPage,photo.from,photo.album,photo.can_delete,hvalue,token)
          // self ! GetFObjestriends                 : TO DO
          failedAttempts = 0
        case Failure(error) =>
        ////println(error, "failed to get photo")
      }

    case AddPage(page:Page) =>
      val token:String = "Token"
      val hvalue:String = AESEncryptor.encrypt(symmetricKey,initVector,page)
      val tempPage: PageWrapper = new PageWrapper(page.id, page.can_checkin,page.can_post,page.username,page.likes,hvalue,token)
      val response: Future[PageWrapper] = pagePipeline(Post("http://localhost:5001/pages/" + name, page))
      response.onComplete {
        case Success(response) =>
          //println("Added Page " + response)
          myHiddenPage = response
          failedAttempts = 0
        case Failure(error) =>
          ////println(error, "failed to get profile")
          if (failedAttempts == 5)
            context.stop(self)
          else failedAttempts += 1
          context.system.scheduler.scheduleOnce(999 milliseconds, self, AddPage(page:Page))
      }



    case AddPhoto(photo:Photo) =>
      val token:String = "Token"
      val hvalue:String = AESEncryptor.encrypt(symmetricKey,initVector,photo)
      val tempPhoto: PhotoWrapper = new PhotoWrapper(photo.id, photo.byPage,photo.from,photo.album,photo.can_delete,hvalue,token)
      val response: Future[PhotoWrapper] = photoPipeline(Post("http://localhost:5001/photos/" + name, tempPhoto))
      response.onComplete {
        case Success(response) =>
          //println("Added Post " + response)
          myHiddenPhoto = response
          failedAttempts = 0
        case Failure(error) =>
          ////println(error, "failed to get profile")
          if (failedAttempts == 5)
            context.stop(self)
          else failedAttempts += 1
          context.system.scheduler.scheduleOnce(999 milliseconds, self, AddPhoto(photo:Photo))
      }

    case AddPost(post:UserPost) =>
      val token:String = "Token"
      val hvalue:String = AESEncryptor.encrypt(symmetricKey,initVector,post)
      val tempPost: UserPostWrapper = new UserPostWrapper(post.id, post.byPage,post.from,post.privacy,hvalue,token)
      val response: Future[UserPostWrapper] = postPipeline(Post("http://localhost:5001/posts/" + name, tempPost))
      response.onComplete {
        case Success(response) =>
          //println("Added Post " + response)
          myHiddenPost = response
          failedAttempts = 0
        case Failure(error) =>
          ////println(error, "failed to get profile")
          if (failedAttempts == 5)
            context.stop(self)
          else failedAttempts += 1
          context.system.scheduler.scheduleOnce(999 milliseconds, self, AddPost(post:UserPost))
      }

    case AddProfile(profile:Profile) =>
      val token:String = "Token"
      val hvalue:String = AESEncryptor.encrypt(symmetricKey,initVector,profile)
      val tempProfile: ProfileWrapper = new ProfileWrapper(profile.id, profile.email,profile.first_name,profile.gender,profile.last_name,hvalue,token)

      val response: Future[ProfileWrapper] = profilePipeline(Post("http://localhost:5001/profiles/" + name, tempProfile))
      response.onComplete {
        case Success(response) =>
          //println("Added Profile " + response)
          //myProfile = response
          failedAttempts = 0
        case Failure(error) =>
          ////println(error, "failed to get profile")
          if (failedAttempts == 5)
            context.stop(self)
          else failedAttempts += 1
          context.system.scheduler.scheduleOnce(999 milliseconds, self, AddProfile(profile:Profile))
      }
  }

  override implicit def json4sFormats: Formats = DefaultFormats
}