pragma SPARK_Mode (On);

with AS_Io_Wrapper; use AS_Io_Wrapper;

-- @Author: Emily Canto

package body aircraft is

   -- Returns if flaps are deployed or retracted
   function Flap_Status_String (Flap : Flap_Status) return String is
   begin
      if Flap = Deployed then
         return "Deployed";
      else
         return "Retracted";
      end if;
   end Flap_Status_String;

   -- Returns if engine is on or off
   function Engine_Status_String (Engine : Engine_Status) return String is
   begin
      if Engine = On then
         return "On";
      else
         return "Off";
      end if;
   end Engine_Status_String;

   -- Returns if yoke is pushed backwards, neutral or forwrds
   function Yoke_Status_String (Yoke : Yoke_Status) return String is
   begin
      if Yoke = Backwards then
         return "Backwards";
      elsif Yoke = Neutral then
         return "Neutral";
      else
         return "Forwards";
      end if;
   end Yoke_Status_String;

   -- Returns if plane has taken off or not
   function Takeoff_Status_String (Takeoff : Takeoff_Status) return String is
   begin
      if Takeoff = Grounded then
         return "Grounded";
      else
         return "Airborne";
      end if;
   end Takeoff_Status_String;

   -- Outputting to plane status to terminal
   procedure Get_Status is
   begin
      AS_Put_Line ("-----");
      AS_Put ("Aircraft Speed = ");
      AS_Put (Integer (Plane_Status.Jet_Plane_Speed));
      AS_Put_Line ("");
      AS_Put ("Engine Status = ");
      AS_Put (Engine_Status_String (Plane_Status.Jet_Engine_Status));
      AS_Put_Line ("");
      AS_Put ("Flaps Status = ");
      AS_Put (Flap_Status_String (Plane_Status.Jet_Flap_Status));
      AS_Put_Line ("");
      AS_Put ("Yoke Status = ");
      AS_Put (Yoke_Status_String (Plane_Status.Jet_Yoke_Status));
      AS_Put_Line ("");
      AS_Put_Line ("-----");
      AS_Put ("TAKEOFF STATUS = ");
      AS_Put (Takeoff_Status_String (Plane_Status.Jet_Takeoff_Status));
      AS_Put_Line ("");
      AS_Put_Line ("-----");
   end Get_Status;

   -- Asks user for engine state and sets engine state based of their answer.
   procedure Set_Engine is
      Option : String (1 .. 20);
      Last   : Integer;
   begin
      loop
         loop
            AS_Put ("Set engine to on, off: ");
            AS_Get_Line (Option, Last);
            exit when Last > 0;
            AS_Put_Line ("Please enter a string");
         end loop;
         exit when Valid_Engine_Option (Option (1 .. 2));
      end loop;
      if Option (1 .. 2) = "on" then
         Plane_Status.Jet_Engine_Status := On;
      else
         Plane_Status.Jet_Engine_Status := Off;
      end if;
   end Set_Engine;

   -- Asks user to set flaps to retracted or deployed, sets flaps to user's reponse.
   procedure Set_Flap is
      Option : String (1 .. 20);
      Last   : Integer;
   begin
      loop
         loop
            AS_Put ("Set flaps to deployed, retracted: ");
            AS_Get_Line (Option, Last);
            exit when Last > 0;
            AS_Put_Line ("Please enter a string");
         end loop;
         exit when Valid_Flap_Option (Option (1 .. 1));
      end loop;
      if Option (1 .. 1) = "d" then
         Plane_Status.Jet_Flap_Status := Deployed;
      else
         Plane_Status.Jet_Flap_Status := Retracted;
      end if;
   end Set_Flap;

   -- Asks user for yoke position, then sets yoke to the user's input.
   procedure Set_Yoke is
      Option : String (1 .. 20);
      Last   : Integer;
   begin
      loop
         loop
            AS_Put ("Set yoke position to backwards, neutral, forwards: ");
            AS_Get_Line (Option, Last);
            exit when Last > 0;
            AS_Put_Line ("Please enter a string");
         end loop;
         exit when Valid_Yoke_Option (Option (1 .. 1));
      end loop;
      if Option (1 .. 1) = "b" then
         Plane_Status.Jet_Yoke_Status := Backwards;
      elsif Option (1 .. 1) = "n" then
         Plane_Status.Jet_Yoke_Status := Neutral;
      else
         Plane_Status.Jet_Yoke_Status := Forwards;
      end if;
   end Set_Yoke;

   -- Set speed, must be between 149 & 177.
   procedure Read_Speed_Sensor is
      Speed : Integer;
   begin
      AS_Put_Line ("Please enter the speed of the aircraft in MPH");
      loop
         AS_Get (Speed, "Please type in a number");
         exit when (Speed >= 0) and (Speed <= Jetliner_Max_Speed);
         AS_Put ("The value must be between 0 and");
         AS_Put (Jetliner_Max_Speed);
         AS_Put_Line ("");
      end loop;
      Plane_Status.Jet_Plane_Speed := Jetliner_Speed_Range (Speed);
   end Read_Speed_Sensor;

   -- Determine if the user's current input has allowed aircraft to takeoff.
   procedure Monitor_Takeoff is
   begin
      if Integer (Plane_Status.Jet_Plane_Speed) >=
        Jetliner_Min_Takeoff_Speed and
        Plane_Status.Jet_Engine_Status = On and
        Plane_Status.Jet_Flap_Status = Deployed and
        Plane_Status.Jet_Yoke_Status = Backwards
      then
         Plane_Status.Jet_Takeoff_Status := Airborne;
      else
         Plane_Status.Jet_Takeoff_Status := Grounded;
      end if;
   end Monitor_Takeoff;

   -- Gives user the options of variables to change.
   -- Checks which option has been picked and then calls it corresponding function.
   procedure Perform_Action is
      Option : String (1 .. 20);
      Last   : Integer;
   begin
      loop
         loop
            AS_Put ("Set engine, flaps, yoke, speed: ");
            AS_Get_Line (Option, Last);
            exit when Last > 0;
            AS_Put_Line ("Please enter a string");
         end loop;
         exit when Valid_Action_Option (Option (1 .. 1));
      end loop;

      if Option (1 .. 1) = "s" then
         Read_Speed_Sensor;
      elsif Option (1 .. 1) = "e" then
         Set_Engine;
      elsif Option (1 .. 1) = "f" then
         Set_Flap;
      else
         Set_Yoke;
      end if;
   end Perform_Action;

   -- Set up of the system, returning all values to default.
   procedure Initialize is
   begin
      AS_Init_Standard_Input;
      AS_Init_Standard_Output;
      Plane_Status :=
        (Jet_Plane_Speed    => 0, Jet_Engine_Status => Off,
         Jet_Flap_Status    => Retracted, Jet_Yoke_Status => Neutral,
         Jet_Takeoff_Status => Grounded);
   end Initialize;

end aircraft;
