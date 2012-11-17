#ifndef ELECTIVECOURSE_H
#define ELECTIVECOURSE_H
#include "course.h"
class ElectiveCourse : public Course {
    public:
        ElectiveCourse(std::vector<std::string> data);
        virtual void reg(Student& s);
};
#endif
