function  Char_Pump_CommandRun()
MatlabInterface = evalin('base','MatlabInterface');
MatlabInterface.key(1,1) = uint8('k');
MatlabInterface.key(1,2) = uint8('d');
MatlabInterface.key(1,3) = uint8('s');
MatlabInterface.key(1,4) = uint8('/');
MatlabInterface.key(1,5) = uint8('l');
MatlabInterface.key(1,6) = uint8('e');
MatlabInterface.key(1,7) = uint8('g');
MatlabInterface.key(1,8) = uint8('a');
MatlabInterface.key(1,9) = uint8('t');
MatlabInterface.key(1,10) = uint8('o');
MatlabInterface.key(1,11) = uint8(' ');
MatlabInterface.key(1,12) = uint8('1');
MatlabInterface.key(1,13) = uint8('1');
MatlabInterface.key(1,14) = uint8('0');
MatlabInterface.key(1,15) = uint8('/');
MatlabInterface.key(1,16) = uint8('c');
MatlabInterface.key(1,17) = uint8('o');
MatlabInterface.key(1,18) = uint8('m');
MatlabInterface.key(1,19) = uint8('m');
MatlabInterface.key(1,20) = uint8('a')
  MatlabInterface.key(1,21) = uint8('n')
  MatlabInterface.key(1,22) = uint8('d')
  MatlabInterface.key(1,23) = 0;
MatlabInterface.key(1,24) = 0;
MatlabInterface.key(1,25) = 0;
MatlabInterface.key(1,26) = 0;
MatlabInterface.key(1,27) = 0;
MatlabInterface.key(1,28) = 0;
MatlabInterface.key(1,29) = 0;
MatlabInterface.key(1,30) = 0;
MatlabInterface.key(1,31) = 0;
MatlabInterface.key(1,32) = 0;

MatlabInterface.value(1,1) = uint8('S');
MatlabInterface.value(1,2) = uint8('E');
MatlabInterface.value(1,3) = uint8('Q');
MatlabInterface.value(1,4) = uint8('#');
MatlabInterface.value(1,5) = uint8('=');
MatlabInterface.value(1,6) = uint8('0');
MatlabInterface.value(1,7) = 13;
MatlabInterface.value(1,8) = uint8('A');
MatlabInterface.value(1,9) = uint8('D');
MatlabInterface.value(1,10) = uint8('D');
MatlabInterface.value(1,11) = uint8('R');
MatlabInterface.value(1,12) = uint8('=');
MatlabInterface.value(1,13) = uint8('0');
MatlabInterface.value(1,14) = uint8('0');
MatlabInterface.value(1,15) = 13;
MatlabInterface.value(1,16) = uint8('R')
  MatlabInterface.value(1,17) = uint8('U')
  MatlabInterface.value(1,18) = uint8('N')
  MatlabInterface.value(1,19) = 13;
MatlabInterface.value(1,20) = 13;
MatlabInterface.value(1,21) = 13;


assignin('base','MatlabInterface',MatlabInterface);

dp = evalin('base','dp');                                                   


for i=1:10
	dp.write(MatlabInterface);
pause(.5);
end

end

