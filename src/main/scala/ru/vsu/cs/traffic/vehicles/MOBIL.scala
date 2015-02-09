package ru.vsu.cs.traffic.vehicles

import ru.vsu.cs.traffic.Vehicle

class MOBIL (vehicle: Vehicle) {

  def lane = vehicle.lane
}

object MOBIL {
  def apply(vehicle: Vehicle) = new MOBIL(vehicle)
}
