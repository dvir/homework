#include "../include/LinkedList.h"
#include <iostream>


/*****************************************************************
 * Link Implementation [Internal class]
 ****************************************************************/

/** constructor
 * A link is a single link in the list - this class never iterates beyond the single link.
 * No deep copy, deep delete - these are all responsibilities of the List.
 */



Link::Link(const std::string& data,  Link* link) : data_(data)
{
   std::cout << "call to: Link::Link(const std::string& data,  Link* link) : data_(data) //Link constructor" << std::endl;
   setNext(link);
}

void Link::setNext(Link* link)
{
   next_=link;
}

Link* Link::getNext() const
{
   return next_;
}

const std::string& Link::getData() const
{
   return data_;
}

/** destructor
 *  No deep deletion for a simple link - this allows us to remove a single link from a list
 */
Link::~Link() {
	std::cout << "call to: Link::~Link() //Link destructor" << std::endl;
}

/** copy constructor
 *  This only copies the data of the link - no deep copy and no copy of next to avoid sharing Links between lists.
 */
Link::Link(const Link& aLink)
{
   std::cout << "call to: Link::Link(const Link& aLink) //Link copy constructor" << std::endl;
   data_=aLink.getData();
   next_=0;
}
/*****************************************************************
 * List Implementation
 ****************************************************************/

/**
 * Constructor.
 * Builds an empty list.
 */
List::List() : head_(0)
{
	std::cout << "call to: List::List() : head_(0) //List constructor" << std::endl;
}

const Link* List::getHead() const
{
   return head_;
}

void List::insertData(const std::string& data)
{
   head_ = new Link(data, head_);
}

/**
 * removes the current head
 * After the method head_ points to the next element
 */
void List::removeFirst()
{
  if (0 != head_) {
    Link* tmp = head_;
    head_ = head_->getNext();
    delete tmp;
  }
}

/**
 * Destructor: "deep delete"
 */
List::~List()
{
  std::cout << "call to: List::~List() //List destructor" << std::endl;
  clear();
}

/**
 * Clear all content (delete all links)
 */
void List::clear()
{
  while (0 != head_) {
    removeFirst();
  }
}
/**
 * deep copy of this list (allocates links)
 */
Link* List::copy() const
{
  if (0 == getHead()) {
    return 0;
  } else {
    Link *head = new Link(*getHead());
    Link *next = head;
    // @inv: next points to last node in new list
    // origPtr points to node in original list that is not yet copied
    for (Link *origPtr = getHead()->getNext(); 0 != origPtr; origPtr = origPtr->getNext()) {
      next->setNext(new Link(*origPtr));
      next = next->getNext();
    }
    return head;
  }
}

/**
 * Copy Constructor:deep copy of aList
 */
List::List(const List &aList)
{
	std::cout << "call to: List::List(const List &aList) //List copy constructor" << std::endl;
    head_ = aList.copy();
}

/**
 * Assignment Operator
 */
List & List::operator=(const List &L)
{
  std::cout << "call to: List & List::operator=(const List &L) //List operator=" << std::endl;
  // check for "self assignment" and do nothing in that case
  if (this == &L) {
    return *this;
  }
  clear();
  head_ = L.copy();
  // return this List
  return *this;
}


