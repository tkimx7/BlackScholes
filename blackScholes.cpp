#include<bits/stdc++.h>

using namespace std;

double N(double d) {
	
	double c1 = 0.5;                  // 1/2
	double c2 = 0.398942280401433;    // 1/(sqrt(2*pi))
	double c3 = -0.0664903800669055;  // -1/(6 * sqrt(2*pi))
	double c4 = 0.00997355701003582;  // 1/(40 * sqrt(2*pi))
	double c5 = -0.00118732821548045; // -1/(336 * sqrt(2*pi))
	double c6 = 0.000115434687616155; //1/(3456 * sqrt(2*pi))
	
	double Z = c6*pow(d,9)+c5*pow(d,7)+c4*pow(d,5)+c3*pow(d,3)+c2*pow(d,1)+c1;
	
	return Z;
}

class V {
	
	double s;
	double k;
	double r;
	double t;
	double v;
	double d1;
	double d2;
	double c;
	double p;
	
	public: 
	
		void setS(double x) { // Spot Rate
			
			s = x;
		}
		void setK(double x) { // Strike
			
			k = x;
		}
		void setR(double x = 0) { // Interest rate, default 0
			
			r = x;
		}
		void setT(double x0 = 0, double x1 = 1) { // Maturity, default 1yr
			
			t = abs(x1 - x0);
		}
		void setV(double x) { // Realized vol
			
			v = x;
		}
		void setD1() { // Z-score, d1
		
			d1 = (log(s/k) + (r + 0.5 * pow(v,2)) * t)/(v * sqrt(t));
		}
		void setD2() { // Z-score, d2
		
			d2 = d1 - v * sqrt(t);
		}
		double getCall() { // Vanilla Call
			
			c = s * N(d1) - k * exp(-r * t) * N(d2);
			
			return c;
		}
		double getPut() { // Vanilla Put
			
			p = k * exp(-r * t) * N(-d2) - s * N(-d1);
			
			return p;
		}
};

int main() {

	V v;
	v.setS(300);
	v.setK(250);
	v.setR();
	v.setT();
	v.setV(0.15);
	v.setD1();
	v.setD2();
	
	cout << v.getCall() << endl;
	cout << v.getPut() << endl;
}
