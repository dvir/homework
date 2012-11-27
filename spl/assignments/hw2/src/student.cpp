#include "../include/typedef.h"
#include "../include/utils.h"
#include "../include/student.h"
#include "../include/course.h"
#include <vector>
#include <string>
#include <cstdlib>
#include <cmath>
#include <fstream>
using namespace std;

Student::Student(std::vector<std::string> data, size_t elective_courses_count):
    _id(atoi(data[0].c_str())),
    _dept(data[1]),
    _image(data[2]),
    _elective_courses_count(elective_courses_count),
    _passed_courses(),
    _current_semester(1),
    _semester_courses(),
    _elective_courses()
{
}

void Student::takeExam(Course& c) {
    if (10*sqrt(rand()%101) < c.getMinGrade()) {
        // the student failed the exam.
        Utils::log(_id, " took ", c.getName(), " and finished UNSUCCESSFULLY");
        return;
    }

    // the student has passed the course successfully!
    _passed_courses.push_back(&c);

    // complete the course (update semester courses vector,
    // elective courses count, etc.)
    completeCourse(c);

    Utils::log(_id, " took ", c.getName(), " and finished SUCCESSFULLY");
}

void Student::completeCourse(Course& c) {
    // if the course was an elective one, 
    // decrease the elective courses count
    if (c.getDept() == "ELECTIVE") {
        // remove the course from elective courses list
        for (Courses::iterator it = _elective_courses.begin();
             it < _elective_courses.end();
             ++it)
        {
            if (c.getName() == (*it)->getName()) {
                _elective_courses.erase(it);
                break;
            }
        }
        return;
    }

    for (Courses::iterator it = _semester_courses.begin();
         it < _semester_courses.end();
         ++it)
    {
        if ((*it)->getName() == c.getName()) {
            _semester_courses.erase(it);
            break;
        }
    }

    if (_semester_courses.size() == 0) {
        _current_semester++;
    }
}

bool Student::hasSemesterCoursesLeft() {
    return (_semester_courses.size() != 0);
}

void Student::addSemesterCourse(Course& c) {
    _semester_courses.push_back(&c);
}

void Student::addElectiveCourse(Course& c) {
    _elective_courses.push_back(&c);
    _elective_courses_count--;
}

void Student::startSemester(size_t semester) {
    if (_current_semester%2 == semester%2) {
        for (Courses::iterator it = _semester_courses.begin();
             it < _semester_courses.end();
             ++it)
        {
            Course* course = *it;

            course->reg(*this);
        }
    }
   
    for (Courses::iterator it = _elective_courses.begin();
         it < _elective_courses.end();
         ++it)
    {
        Course* course = *it;

        if (semester%2 == course->getSemester()%2) {
            course->reg(*this);
        }
    }
}

bool Student::hasCompleted(Course& c) {
    for (Courses::iterator it = _passed_courses.begin();
         it < _passed_courses.end();
         ++it)
    {
        if ((*it)->getName() == c.getName()) {
            return true;
        }
    }

    return false;
}

bool Student::hasGraduated(size_t dept_courses_count) {
    if (_elective_courses_count > 0) {
        // the student still has elective courses 
        // to take before graduating.
        return false;
    }

    if (_elective_courses.size() > 0) {
        // the student still has elective courses 
        // TO FINISH before graduating.
        return false;
    }

    if (dept_courses_count > _passed_courses.size()) { 
        // the student didn't finish all the dept courses,
        // so he didn't graduate yet.
        return false;
    }

    // if we got here, the student finished all the required elective
    // courses and all of the dept courses.
    // so, he graduated from Random University!
    return true;
}
