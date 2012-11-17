#ifndef STUDENT_H
#define STUDENT_H
#include <string>
#include <vector>
#include <fstream>
#include "typedef.h"

class Student {
    public:
        Student(std::string student_id);
        void addCourse(Course* course);
        void print(std::string filename);
        std::string getStudentId() const;

    private:
        std::string _student_id;
        Courses _courses;
};
#endif
