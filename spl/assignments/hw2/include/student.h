#ifndef STUDENT_H
#define STUDENT_H
#include "typedef.h"
#include <vector>
#include <string>
class Student {
    public:
        Student(std::vector<std::string> data, size_t elective_courses_count);
        virtual ~Student() { };
        virtual void study(Course& c) = 0;
        virtual size_t getId() { return _id; };
        virtual std::string getDept() { return _dept; };
        virtual std::string getImage() { return _image; };
        virtual size_t getElectiveCoursesCount() { return _elective_courses_count; };
        virtual size_t getCurrentSemester() { return _current_semester; };
        virtual bool hasSemesterCoursesLeft();
        virtual void addSemesterCourse(Course& c);
        virtual void addElectiveCourse(Course& c);
        virtual void startSemester(size_t semester);
        virtual bool hasCompleted(Course& c);
        virtual bool hasGraduated(size_t dept_courses_count);

    protected:
        size_t _id;
        std::string _dept;
        std::string _image;
        size_t _elective_courses_count;
        Courses _passed_courses;
        size_t _current_semester;
        Courses _semester_courses;
        Courses _elective_courses;

        virtual void takeExam(Course& c);
        virtual void completeCourse(Course& c);
};
#endif
