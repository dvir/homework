#ifndef TYPEDEF_H
#define TYPEDEF_H
#include <vector>

class Student;
typedef std::vector<Student*> Students;

class CSStudent;
class PGStudent;


class Course;
typedef std::vector<Course*> Courses;
class CSCourse;
class PGCourse;
class ElectiveCourse;

#endif
