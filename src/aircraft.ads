pragma SPARK_Mode (On);

with SPARK.Text_IO; use SPARK.Text_IO;

-- @Author: Emily Canto

package aircraft is
   -- This relates to a sensor outputting the recorded speed of the aircraft.
   -- Wikipedia specifies the average take-off speed of a jetliner is between 149mph & 177mph.
   -- As a result, I have set the minimum for takeoff at 149 and maximum at 177.
   Jetliner_Max_Speed         : constant Integer := 177;
   Jetliner_Min_Takeoff_Speed : constant Integer := 149;

   -- This variable relates to a sensor determining that wing flaps have both been deployed and are working correctly.
   -- These must be deployed to increase lift.
   -- If not deployed, the aircraft may not takeoff before the end of the runway.
   type Flap_Status is (Deployed, Retracted);

   -- Relates to not only the jetliners multiple engines being on, but that they're also within expected values e.g temperature
   -- Failure could cause issues such as aircraft simply not moving at all, or the worst case of engine failure mid take-off.
   type Engine_Status is (On, Off);

   -- This correlates to the physical Yoke being pulled back by the pilot.
   -- Must be pulled backwards to cause the nose to lift up off the runway.
   -- Without this step, the aircraft will not takeoff due to incorrect angle.
   type Yoke_Status is (Backwards, Neutral, Forwards);

   type Takeoff_Status is (Grounded, Airborne);

   type Jetliner_Speed_Range is new Integer range 0 .. Jetliner_Max_Speed;

   -- Stores the current status of the jetliner.
   type Jetliner_Plane_Status is record
      Jet_Plane_Speed    : Jetliner_Speed_Range;
      Jet_Engine_Status  : Engine_Status;
      Jet_Flap_Status    : Flap_Status;
      Jet_Yoke_Status    : Yoke_Status;
      Jet_Takeoff_Status : Takeoff_Status;
   end record;

   Plane_Status : Jetliner_Plane_Status;

   -- Determine if the input for performing an action is correct
   -- s = set speed, e = set engine, f = set flaps, y = set yoke
   function Valid_Action_Option (Option : String) return Boolean is
     (Option = "s" or Option = "e" or Option = "f" or Option = "y");

   -- Determine if the input for setting Engine is correct
   -- on = on, of = off
   function Valid_Engine_Option (Option : String) return Boolean is
     (Option = "on" or Option = "of");

   -- Determine if the input for setting Flap is correct
   -- d = deployed, r = retracted
   function Valid_Flap_Option (Option : String) return Boolean is
     (Option = "d" or Option = "r");

   -- Determine if the input for setting Yoke is correct
   -- b = backwards, n = neutral, f = forwards
   function Valid_Yoke_Option (Option : String) return Boolean is
     (Option = "b" or Option = "n" or Option = "f");

   --Expression function that determines if all conditions are met to safely takeoff, if not, plane will stay grounded.
   function Is_Safe (Status : Jetliner_Plane_Status) return Boolean is
     (if
        Status.Jet_Flap_Status = Deployed and Status.Jet_Engine_Status = On and
        Status.Jet_Yoke_Status = Backwards and
        Integer (Status.Jet_Plane_Speed) >= Jetliner_Min_Takeoff_Speed
      then Status.Jet_Takeoff_Status = Airborne
      elsif Integer (Status.Jet_Plane_Speed) < Jetliner_Min_Takeoff_Speed then
        Status.Jet_Takeoff_Status = Grounded);

   function Flap_Status_String (Flap : Flap_Status) return String;

   function Engine_Status_String (Engine : Engine_Status) return String;

   function Yoke_Status_String (Yoke : Yoke_Status) return String;

   function Takeoff_Status_String (Takeoff : Takeoff_Status) return String;

   procedure Get_Status with
      Global  => (In_Out => Standard_Output, Input => Plane_Status),
      Depends => (Standard_Output => (Standard_Output, Plane_Status));

   procedure Set_Engine with
      Global  => (In_Out => (Standard_Input, Standard_Output, Plane_Status)),
      Depends => (Standard_Output => (Standard_Output, Standard_Input),
       Standard_Input => Standard_Input,
       Plane_Status   => (Plane_Status, Standard_Input));

   procedure Set_Flap with
      Global  => (In_Out => (Standard_Input, Standard_Output, Plane_Status)),
      Depends => (Standard_Output => (Standard_Output, Standard_Input),
       Standard_Input => Standard_Input,
       Plane_Status   => (Plane_Status, Standard_Input));

   procedure Set_Yoke with
      Global  => (In_Out => (Standard_Input, Standard_Output, Plane_Status)),
      Depends => (Standard_Output => (Standard_Output, Standard_Input),
       Standard_Input => Standard_Input,
       Plane_Status   => (Plane_Status, Standard_Input));

   procedure Read_Speed_Sensor with
      Global  => (In_Out => (Standard_Input, Standard_Output, Plane_Status)),
      Depends => (Standard_Output => (Standard_Output, Standard_Input),
       Standard_Input => Standard_Input,
       Plane_Status   => (Plane_Status, Standard_Input));

   procedure Monitor_Takeoff with
      Global  => (In_Out => Plane_Status),
      Depends => (Plane_Status => Plane_Status),
      Post    => Is_Safe (Plane_Status);

   procedure Perform_Action with
      Global  => (In_Out => (Standard_Output, Standard_Input, Plane_Status)),
      Depends => (Standard_Output => (Standard_Output, Standard_Input),
       Standard_Input => Standard_Input,
       Plane_Status   => (Plane_Status, Standard_Input));

   procedure Initialize with
      Global  => (Output => (Standard_Output, Standard_Input, Plane_Status)),
      Depends => ((Standard_Output, Standard_Input, Plane_Status) => null),
      Post    => Is_Safe (Plane_Status);

end aircraft;
