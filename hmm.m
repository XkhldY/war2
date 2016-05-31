
% trainingSequencesOnly(isnan(trainingSequencesOnly(1,:)),:)=[];


% out = mat2cell(trainingSequencesOnly,ones(1,size(trainingSequencesOnly,1)),size(trainingSequencesOnly,2));


%[estTR,estE] = hmmtrain(out,StateTransitions,Emissions);

% out2Copy(any(cellfun(@(x) any(isnan(x)),out2Copy),2),:) = [];

   
%    hds = table2array(out1{1,1}(1,:));

%  out1{1,1}(1,:)


%fid = fopen('D:\work\Research\location research\datasets\Geolife\trainingSequencesOnly.csv','r');
fid = fopen('D:\work\Research\location research\datasets\Geolife\trainingSequenceslatest.csv','r');
tline = fgets(fid);
 y={}
 i=1
tline = fgets(fid);
while ischar(tline)
%while i<16    
    z=regexprep(tline,'[^0-9\s+-.eE]','');
    y(1,i)={str2num(z)};
    tline = fgets(fid);
    i=i+1;
end

fclose(fid);
% tline = fgetl(fid);
% while (~isempty(tline) )
% tline = fgetl(fid);
% tline
% end
% linesToSkip = 1;
% for ii = 1:linesToSkip-1
%     fgetl(fid);
% end

%     fgetl(fid);
% tline =  fgetl(fid);
% your_data = []; %You should allocate if you know how large your data is
%  while (~isempty(tline) )
 %   tline = fgetl(fid);
    %Getting rid of non-numbers
 %   tline = regexprep(tline,'[^0-9\s+-.eE]','');
 %   your_data = str2num(tline);
%  end
