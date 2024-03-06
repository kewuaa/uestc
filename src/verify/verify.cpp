#ifdef _WIN32
#define EXPORT extern "C" __declspec(dllexport)
#elif __linux__
#define EXPORT extern "C" __attribute__((visibility("default")))
#endif
#include "canny.hpp"

typedef Matrix<double, Dynamic, Dynamic, RowMajor> MatrixXdRowMajor;


EXPORT double calculate_move_length(
    int fg_width,
    int bg_width,
    int height,
    double* fg_data,
    double* bg_data
) {
    MatrixXd fg = Map<MatrixXdRowMajor>(fg_data, height, fg_width);
    MatrixXd bg = Map<MatrixXdRowMajor>(bg_data, height, bg_width);
    auto mask = (fg.array() > 0).select(
            MatrixXd::Ones(fg.rows(), fg.cols()),
            MatrixXd::Zero(fg.rows(), fg.cols())
            );
    canny(fg, 180, 200);
    canny(bg, 180, 200);
    int answer_i;
    double max = 0;
    for (int i = 0; i < bg.cols() - fg.cols(); i++) {
        auto block = bg.block(0, i, fg.rows(), fg.cols());
        auto diff = (block.array() * mask.array() * fg.array()).sum();
        if (diff > max) {
            answer_i = i;
            max = diff;
        }
    }
    return (double)answer_i / bg.cols();
}
