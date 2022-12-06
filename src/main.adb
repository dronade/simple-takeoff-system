pragma SPARK_Mode (on);

with aircraft; use aircraft;

-- @Author: Emily Canto

-- This system models a few of the sensors in a jetliner that determine takeoff.
-- I have specified a jetliner due to different types of aircraft having different take-off speeds and wanted to focus on one.

procedure Main is

begin
   Initialize;
   Monitor_Takeoff;
   loop
      -- Plane is always in a safe state during loop.
      pragma Loop_Invariant (Is_Safe (Plane_Status));
      Perform_Action;
      Monitor_Takeoff;
      Get_Status;
   end loop;

end Main;
