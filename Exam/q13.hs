-- Q13
data Student = Student { sname:: String , courseLeader:: Maybe Lecturer} deriving Show

student1 = Student {sname = "student1", courseLeader = Just rob}
student2 = Student {sname = "potentialstudent1", courseLeader = Nothing}
student3 = Student {sname = "student3", courseLeader = Just john}

data Lecturer = Lecturer {lname :: String , roomNumber :: Maybe String} deriving Show
rob = Lecturer {lname = "Robert", roomNumber = Just "311"}
john = Lecturer { lname = "John", roomNumber = Nothing}

-- Part 1.
getLeaderRoom :: Student -> Maybe String
getLeaderRoom std = getRoom $ courseLeader std

getRoom :: Maybe Lecturer -> Maybe String
getRoom Nothing = Nothing
getRoom (Just lec) = roomNumber lec

-- Part 2.
getStudentLeader :: Student -> String
getStudentLeader std = getLecturer $ courseLeader std

getLecturer :: Maybe Lecturer -> String
getLecturer Nothing = ""
getLecturer (Just lec) = lname lec