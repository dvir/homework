#ifndef STUDENT_H
#define STUDENT_H
#include "typedef.h"
#include <vector>
#include <string>
class Student {
    public:
        Student(std::vector<std::string> data, int elective_courses_count);
        virtual ~Student() { };
        virtual void study(Course& c) = 0;
        virtual int getId() { return _id; };
        virtual std::string getDept() { return _dept; };
        virtual std::string getImage() { return _image; };
        virtual int getElectiveCoursesCount() { return _elective_courses_count; };
        virtual int getCurrentSemester() { return _current_semester; };
        virtual bool hasSemesterCoursesLeft();
        virtual void addSemesterCourse(Course& c);
        virtual void startSemester(int semester);
        virtual bool hasCompleted(Course& c);
        virtual bool hasGraduated(size_t dept_courses_count);

    protected:
        int _id;
        std::string _dept;
        std::string _image;
        int _elective_courses_count;
        Courses _passed_courses;
        int _current_semester;
        Courses _semester_courses;

        virtual void takeExam(Course& c);
        virtual void completeCourse(Course& c);
};
#endif
