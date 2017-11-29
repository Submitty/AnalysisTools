module Lichen.Plagiarism.Submitty where

import qualified Data.Text as T
import Lichen.Lexer

data Semester = Semester { semesterName :: T.Text, semesterCourses :: [Course] }
data Course = Course { courseName :: T.Text, courseAssignments :: [Assignment] }
data Assignment = Assignent { assignmentName :: T.Text, assignmentStudents :: [Student] }
data Student = Student { studentName :: T.Text, studentActiveSubmission :: Integer, studentSubmissions :: [Submission] }
data Submission = Submission { submissionVersion :: Integer, submissionFingerprints :: [Fingerprint] }
type Fingerprint = Tagged Int
