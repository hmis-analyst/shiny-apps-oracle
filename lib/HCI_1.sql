/*
Creates a table called HCI_1 that summarizes household information and links it to program enrollments.
Contents: new household (hh) variables.
  Num_Fam: number of hh members
  Num_Child: number of children in hh
  Num_Adult: number of adults in hh
  Num_Teen_M: number of teenage males in hh
  Num_Teen_F: number of teentage females in hh
  Head_Num: Number of head of hh
  Head_Age: The age of the oldest head of hh
  Fem_Head: Number of female head of hh
  Oldest_Age: Age of the oldest hh member
  Oldest_Fem_Age: Age of the oldest female hh member
  Oldest_DoB: Oldest hh member's date of birth
Each of these variables can be called in the main SELECT clause that the JOIN is attached to.
*/
LEFT JOIN (
	SELECT 
		Program_Enrollment_Key,
		count(Client_Key) Num_Fam, 
		count(Child) Num_Child, 
		count(Adult) Num_Adult, 
		count(Teen_M) Num_Teen_M,
		count(Teen_F) Num_Teen_F,
		count(Head) Head_Num,
		max(Head_Age) Head_Age,
		count(Fem_Head) Fem_Head,
		max(Age) Oldest_Age,
		max(Oldest_Fem_Age) Oldest_Fem_Age,
		min(Date_of_Birth) Oldest_DoB
	FROM (
		SELECT unique
			Program_Enrollment_Key, 
  		HCI.Client_Key, 
			case when (Program_Entry_Date - Date_of_Birth)/365.25 < 18 then 1 end Child, 
			case when (Program_Entry_Date - Date_of_Birth)/365.25 >= 18 then 1 end Adult, 
			case when Relationship_Code = 3 then (Program_Entry_Date - Date_of_Birth)/365.25 end Head_Age,
	  	case when Relationship_Code = 3 then 1 end Head,
			case when Relationship_Code = 3 and Gender_Code = 2 then 1 end Fem_Head,
			case when Gender_Code = 2 then (Program_Entry_Date - Date_of_Birth)/365.25 else 0 end Oldest_Fem_Age,
			case when (Program_Entry_Date - Date_of_Birth)/365.25 >= 13 and (Program_Entry_Date - Date_of_Birth)/365.25 < 20 and Gender_Code = 1 then 1 end Teen_M,
			case when (Program_Entry_Date - Date_of_Birth)/365.25 >= 13 and (Program_Entry_Date - Date_of_Birth)/365.25 < 20 and Gender_Code = 2 then 1 end Teen_F,
			(Program_Entry_Date - Date_of_Birth)/365.25 Age,
			Date_of_Birth
		FROM Household_Client_Info HCI
		JOIN Client_Information CI 
			on HCI.Client_Key = CI.Client_Key
		JOIN Program_Enrollment PE
			on HCI.Household_Key = PE.Household_Key)
		GROUP BY Program_Enrollment_Key
) HCI_1
	on PE.Program_Enrollment_Key = HCI_1.Program_Enrollment_Key
