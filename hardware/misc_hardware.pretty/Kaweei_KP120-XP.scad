$fn = 50;

ports = 3;

b = 2.54 * (ports - 1);
a = 2.92 + b;

translate([0, -b / 2, 0])
rotate([90, 0, 180])
{
  color([0.4, 0.4, 0.4])
  difference() {
    translate([0, 0, -1.46])
    linear_extrude(a)
    polygon([[0, 0],
             [0, 5.13],
             [0.81, 5.13],
             [1.72, 8.65],
             [4.58, 8.65],
             [6.3, 6],
             [6.3, 0]]);

    for (n = [0 : (ports - 1)])
    translate([0, 0.2, 2.54 * n])
    rotate([0, 90, 0])
    union() {
      hull() {
        translate([0, 4.45 / 2, 0.25])
        cube([2.2, 4.45, 0.8], center = true);

        translate([0, 2.12, 0.7 + (0.53 / 2)])
        cube([1.4, 2, 0.53], center = true);
      }

      translate([0, 2.12, 1])
      cube([1.4, 2, 2], center = true);

      translate([0, 2.12, 2.68])
      cube([1.96, 2.8, 2.9], center = true);

      translate([0, 4, 3.15])
      rotate([-90, 0, 0])
      cylinder(5, r = 1.05);
    }
  }

  color([0.8, 0.82, 0.94])
  for (n = [0 : (ports - 1)])
  translate([0, 0, 2.54 * n])
  union() {
    difference() {
      translate([2.68, 2.32, 0])
      cube([2.9, 2.8, 1.96], center = true);

      translate([4.1 / 2, 2.32, 0])
      cube([4.1, 1.5, 1.1], center = true);
    }

    difference() {
      translate([3.15, 4.2, 0])
      rotate([-90, 0, 0])
      cylinder(4.3, r = .9);

      translate([3.15, 8.55, 0])
      cube([2, 0.8, 0.4], center = true);
    }

    translate([3.25, 0, 0])
    rotate([0, 90, 0])
    linear_extrude(0.5)
    polygon([[-0.4, 0],
             [-0.4, -3.1],
             [-0.1, -3.5],
             [0.1, -3.5],
             [0.4, -3.1],
             [0.4, 0]]);
  }
}
