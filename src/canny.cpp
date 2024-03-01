#define PI 3.14159265
#include <iostream>
#include <math.h>

#include "canny.hpp"


void special_filter(MatrixXd& mat, const MatrixXd& kernel) {
    int rows = mat.rows();
    int cols = mat.cols();
    MatrixXd _mat = mat;
    for (int i = 1; i < rows - 1; i++) {
        for (int j = 1; j < cols - 1; j++) {
            mat(i, j) = (_mat.block(i - 1, j - 1, 3, 3).array() * kernel.array()).sum();
        }
    }
    mat.row(0) = mat.row(1);
    mat.row(rows - 1) = mat.row(rows - 2);
    mat.col(0) = mat.col(1);
    mat.col(cols - 1) = mat.col(cols - 2);
}


void init_gaussian_kernel(MatrixXd& kernel, double sigma, bool norm) {
    int rows = kernel.rows();
    int cols = kernel.cols();
    double x = (rows - 1) / 2.;
    double y = (cols - 1) / 2.;
    double pow_sigma = pow(sigma, 2.) * 2;
    double s = 0.;
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            kernel(i, j) = exp(-(pow(i - x, 2.) + pow(j - y, 2.)) / pow_sigma) / PI / pow_sigma;
            s += kernel(i, j);
        }
    }
    if (norm) {
        kernel /= s;
    }
}


void canny(MatrixXd& img, unsigned char low_threshold, unsigned char high_threshold) {
    auto rows = img.rows();
    auto cols = img.cols();
    MatrixXd kernel = MatrixXd::Zero(3, 3);
    init_gaussian_kernel(kernel, 3, true);
    special_filter(img, kernel);
    kernel << -1, 0, 1,
              -2, 0, 2,
              -1, 0, 1;
    MatrixXd dx = img, dy = img;
    special_filter(dx, kernel);
    special_filter(dy, kernel.transpose());
    MatrixXd grad = (dx.array().pow(2.) + dy.array().pow(2.)).sqrt().matrix();
    img = grad;
    for (int i = 1; i < rows - 1; i++) {
        for (int j = 1; j < cols - 1; j++) {
            auto pixel = grad(i, j);
            double p1, p2;
            if (dx(i, j) == 0) {
                p1 = grad(i, j - 1);
                p2 = grad(i, j + 1);
            } else if (dy(i, j) == 0) {
                p1 = grad(i - 1, j);
                p2 = grad(i + 1, j);
            } else {
                double slope = dy(i, j) / dx(i, j);
                if (slope > 0) {
                    p1 = grad(i + 1, j + 1);
                    p2 = grad(i - 1, j - 1);
                } else {
                    p1 = grad(i - 1, j + 1);
                    p2 = grad(i + 1, j - 1);
                }
            }
            if (p1 > pixel || p2 > pixel) {
                img(i, j) = 0.;
            }
        }
    }
    img.row(0).setZero();
    img.row(rows - 1).setZero();
    img.col(0).setZero();
    img.col(cols - 1).setZero();
    grad = img;
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            auto pixel = grad(i, j);
            if (pixel <= low_threshold) {
                img(i, j) = 0.;
            } else if (pixel > high_threshold) {
                img(i, j) = 255.;
            } else {
                img(i ,j) = 0.;
                for (int k = 0; k < 3; k++) {
                    int l = i - 1 + k;
                    if (grad(l, j - 1) > high_threshold || grad(l, j) > high_threshold || grad(l, j + 1) > high_threshold) {
                        img(i, j) = 255.;
                        break;
                    }
                }
            }
        }
    }
}
