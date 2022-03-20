package u03

  enum Person:
    case Student (name: String, year: Int)
    case Teacher (name: String, course: String)
  
  object Person {
    
    def name (p: Person): String = p match
     case Student (n , _) => n
     case Teacher (n , _) => n

    def teacherCourse (p: Person): String = p match
      case Teacher (_ , n) => n
      case _ => ""
        
    def isTeacher(p: Person): Boolean = p match 
      case Teacher(_,_) => true
      case _ => false

    def isStudent(p: Person): Boolean = p match
      case Student(_,_) => true
      case _ => false   
      
}
