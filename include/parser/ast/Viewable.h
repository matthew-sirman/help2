//
// Created by matthew on 24/12/2020.
//

#ifndef HELP2_VIEWABLE_H
#define HELP2_VIEWABLE_H

template<typename NodeT>
concept Viewable = requires {
    typename NodeT::View;
};

#endif //HELP2_VIEWABLE_H
