courseLeaderOfStudent :: Student -> Maybe Lecturer 
courseLeaderOfStudent st = getLeader (course st)

getLeader:: Maybe Course -> Maybe Lecturer
getLeader (Just c) = leader c
getLeader Nothing = Nothing 

officeOfLeader :: Student -> Maybe String
officeOfLeader st = getRoom $ getLeader (course st)

getRoom:: Maybe Lecturer -> Maybe String
getRoom (Just l) = roomNumber l
getRoom Nothing = Nothing