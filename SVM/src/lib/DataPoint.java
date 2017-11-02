

public class DataPoint {

 double dynalign;
 double shortSeqLength;
 double aFreqOfSeqOne;
 double uFreqOfSeqOne;
 double cFreqOfSeqOne;
 double aFreqOfSeqTwo;
 double uFreqOfSeqTwo;
 double cFreqOfSeqTwo;

 double desired;
 double yeild;

    @Override
    public String toString() {
        return "DataPoint{" +
                "dynalign=" + dynalign +
                ", shortSeqLength=" + shortSeqLength +
                ", aFreqOfSeqOne=" + aFreqOfSeqOne +
                ", uFreqOfSeqOne=" + uFreqOfSeqOne +
                ", cFreqOfSeqOne=" + cFreqOfSeqOne +
                ", aFreqOfSeqTwo=" + aFreqOfSeqTwo +
                ", uFreqOfSeqTwo=" + uFreqOfSeqTwo +
                ", cFreqOfSeqTwo=" + cFreqOfSeqTwo +
                ", desired=" + desired +
                ", yeild=" + yeild +
                '}';
    }

    public DataPoint(double dynalign,
                     double shortSeqLength,
                     double aFreqOfSeqOne,
                     double uFreqOfSeqOne,
                     double cFreqOfSeqOne,
                     double aFreqOfSeqTwo,
                     double uFreqOfSeqTwo,
                     double cFreqOfSeqTwo,
                     double desired) {
        this.dynalign = dynalign;
        this.shortSeqLength = shortSeqLength;
        this.aFreqOfSeqOne = aFreqOfSeqOne;
        this.uFreqOfSeqOne = uFreqOfSeqOne;
        this.cFreqOfSeqOne = cFreqOfSeqOne;
        this.aFreqOfSeqTwo = aFreqOfSeqTwo;
        this.uFreqOfSeqTwo = uFreqOfSeqTwo;
        this.cFreqOfSeqTwo = cFreqOfSeqTwo;
        this.desired = desired;
    }

    public DataPoint(){
        this.yeild = Integer.MIN_VALUE;

        this.dynalign = 0;
        this.shortSeqLength = 0;
        this.aFreqOfSeqOne = 0;
        this.uFreqOfSeqOne = 0;
        this.cFreqOfSeqOne = 0;
        this.aFreqOfSeqTwo = 0;
        this.uFreqOfSeqTwo = 0;
        this.cFreqOfSeqTwo = 0;
    }


}
