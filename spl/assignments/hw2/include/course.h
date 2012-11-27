#ifndef COURSE_H
#define COURSE_H
#include "typedef.h"
#include <vector>
#include <string>
class Course {
    public:
        Course(std::vector<std::string> data);
        virtual ~Course() { };
        virtual void teach();
        virtual void reg(Student& s) = 0;
        virtual std::string getDept() { return _dept; };
        virtual std::string getName() { return _name; };
        virtual size_t getSemester() { return _semester; };
        virtual size_t getMinGrade() { return _min_grade; };

    protected:
        Students _students;
        std::string _dept;
        std::string _name;
        size_t _semester;
        size_t _min_grade;

        virtual void reset();
};
#endif
