#include "Eigen/Core"
using namespace Eigen;

void special_filter(MatrixXd& mat, const MatrixXd& kernel);
void init_gaussian_kernel(MatrixXd& kernel, double sigma, bool norm);
void canny(MatrixXd& img, unsigned char low_threshold, unsigned char high_threshold);
