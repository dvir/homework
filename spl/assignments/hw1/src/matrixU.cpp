#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <cstdlib>
#include "../include/typedef.h"
#include "../include/student.h"
#include "../include/course.h"
using namespace std;

vector<string> str_split(string str, char separator) {
	vector<string> words;
	string word = ""; 
    string::iterator it;
	for (it = str.begin(); it < str.end(); ++it) {
		if (*it == separator) {
			words.push_back(word);
			word = "";
		} else {
			word = word + *it;
		}
	}

	if (word != "") words.push_back(word);
	
	return words;
}

int main(int argc, char* argv[]) {
    ifstream file;
    string data;
    vector<string> tokens;
    Courses courses;
    Students students;

    file.open("courses.conf");
    if (!file.is_open()) {
        cout << "Failed opening courses.conf." << endl;
        return 0;
    }
    while (getline(file, data)) {
        tokens = str_split(data, ',');
        // data: WEEKDAY,COURSE-ID,SPACE
        Course* course = new Course(*(tokens[0].c_str()) - '0', 
                                    tokens[1], 
                                    atoi(tokens[2].c_str()));
        courses.push_back(course);
    }
    file.close();

    file.open("students.conf");
    if (!file.is_open()) {
        cout << "Failed opening students.conf." << endl;
        return 0;
    }

    while (getline(file, data)) {
        tokens = str_split(data, ',');

        // data: STUD-ID,COURSE-ID1,COURSE-ID2,...,COURSE-IDN
        Student* student = new Student(tokens[0]);
        if (tokens.size() > 1) {
            for (size_t i = 1; i < tokens.size(); ++i) {
                for (size_t j = 0; j < courses.size(); ++j) {
                    if (courses[j]->getCourseId() == tokens[i]) {
                        if (courses[j]->addStudent(student)) {
                            // we have found a room for this student
                            // in the course he requested, so we are breaking
                            // the search for room.
                            // NOTE: we are not break;'ing when we find 
                            // a matching course because there might be 
                            // room in another day for this course.
                            break;
                        }
                    }
                }
            }
        }

        students.push_back(student);
    }
    
    file.close();

    // clean output files
    ofstream output;
    output.open("courses.out");
    output.close();
    output.open("students.out");
    output.close();

    for (size_t i = 0; i < courses.size(); ++i) {
        courses[i]->print("courses.out");
    }

    for (size_t i = 0; i < students.size(); ++i) {
        students[i]->print("students.out");
    }

    for (size_t i = 0; i < courses.size(); ++i) {
        delete courses[i];
        courses[i] = 0;
    }

    for (size_t i = 0; i < students.size(); ++i) {
        delete students[i];
        students[i] = 0;
    }

    return 0;
}
