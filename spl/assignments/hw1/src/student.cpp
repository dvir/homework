#include <string>
#include <iostream>
#include <vector>
#include "../include/student.h"
#include "../include/course.h"
using namespace std;

Student::Student(string student_id):
    _student_id(student_id),
    _courses()
{
}

void Student::addCourse(Course* course) {
    vector<Course*>::iterator iter;
    for (iter = _courses.begin(); iter < _courses.end(); ++iter) {
        if ((*iter)->getWeekday() > course->getWeekday()) {
            break;
        }
    }

    _courses.insert(iter, course);
}

void Student::print(string filename) {
    ofstream stream;
    stream.open(filename.c_str(), ios::app);
    stream << _student_id << endl;
    for (size_t i = 0; i < _courses.size(); ++i) {
        stream << _courses[i]->getWeekday() << " ";
        stream << _courses[i]->getCourseId() << endl;
    }

    stream.close();
}

string Student::getStudentId() const {
    return _student_id;
}
